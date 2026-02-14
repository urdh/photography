ARG HASKELL_VERSION=9.6.7
ARG DEBIAN_VERSION=bullseye

FROM haskell:${HASKELL_VERSION}-slim-${DEBIAN_VERSION} AS base

# Create build directory
RUN mkdir -p /opt/build
WORKDIR /opt/build

# Force stack to use the system GHC
RUN mkdir -p /etc/stack
RUN echo "system-ghc: true" >> /etc/stack/config.yaml

# Copy the stack defs and build all deps
COPY stack.yaml stack.yaml.lock photography.cabal /opt/build/
RUN stack build --only-snapshot

FROM haskell:${HASKELL_VERSION}-${DEBIAN_VERSION} AS devcontainer
ARG HASKELL_LANGUAGE_SERVER_VERSION=2.13.0.0
ARG HASKELL_VERSION

# Install OpenSSH and locales
RUN <<-EOF
    apt-get update
    apt-get install -y --no-install-recommends \
        git-lfs        \
        locales        \
        openssh-client
    rm -rf /var/lib/apt/lists/*
EOF

# Set up a different user for the devcontainer
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=1000

RUN <<-EOF
    groupadd -g ${USER_GID} ${USERNAME}
    useradd -s /bin/bash -m -d /home/${USERNAME} -u ${USER_UID} -g ${USER_GID} ${USERNAME}
EOF

# Also set up a sensible locale
ARG LANGUAGE=en_US
ARG ENCODING=UTF-8

RUN localedef -i ${LANGUAGE} -c -f ${ENCODING} -A /usr/share/locale/locale.alias ${LANGUAGE}.${ENCODING}
ENV LANG=${LANGUAGE}.${ENCODING}

# Install the Haskell language server from github
RUN <<-EOF
    . /etc/os-release
    HLS_VERSION=${HASKELL_LANGUAGE_SERVER_VERSION}
    HLS_PACKAGE_URL=https://downloads.haskell.org/~hls/haskell-language-server-${HLS_VERSION}/haskell-language-server-${HLS_VERSION}-x86_64-linux-deb${VERSION_ID}.tar.xz
    HLS_PACKAGE_DIR=`mktemp -d`
    curl -s -L ${HLS_PACKAGE_URL} | tar -x -J -C ${HLS_PACKAGE_DIR} --strip-components=1      \
        haskell-language-server-${HLS_VERSION}/haskell-language-server-${HASKELL_VERSION}.in  \
        haskell-language-server-${HLS_VERSION}/bin/haskell-language-server-${HASKELL_VERSION} \
        haskell-language-server-${HLS_VERSION}/bin/haskell-language-server-wrapper            \
        haskell-language-server-${HLS_VERSION}/lib/${HASKELL_VERSION}                         \
        haskell-language-server-${HLS_VERSION}/scripts                                        \
        haskell-language-server-${HLS_VERSION}/GNUmakefile
    make -C ${HLS_PACKAGE_DIR} PREFIX=/opt/hls install
EOF
ENV PATH="/opt/hls/bin:$PATH"

# Copy the settings & dependencies from the base image
COPY --from=base /etc/stack/config.yaml /etc/stack/config.yaml
COPY --chown=${USER_UID}:${USER_GID} \
    --from=base /root/.stack /${USERNAME}/.stack

CMD ["sleep", "infinity"]

FROM base AS builder

# Copy the project sources, then build a static executable
RUN --mount=type=bind,source=src,target=/opt/build/src \
    --mount=type=cache,target=/opt/build/.stack-work \
    stack install --only-locals --local-bin-path /usr/bin/

FROM builder AS generator

# Build the actual site using the `site` binary
WORKDIR /workspace
RUN --mount=type=bind,source=provider,target=/workspace/provider \
    --mount=type=cache,target=/opt/build/_cache \
    /usr/bin/site rebuild

FROM joseluisq/static-web-server:2 AS runtime

COPY --from=generator /workspace/_site /var/public
ENTRYPOINT ["/static-web-server", "--root=/var/public", "--health", \
    "--cors-allow-origins", "https://sigurdhsson.org"]
