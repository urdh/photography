# photography

This is the source of my photography showcase, [photography.sigurdhsson.org](https://photography.sigurdhsson.org).
The code itself, including the template and style sheets, are made available under the [ISC license](LICENSE.md)
unless otherwise noted. The photographies themselves (which are stored in a private submodule) are *not* subject
to this license.

## Building

To build the site, you need [Stack](https://www.haskellstack.org/). The whole thing is written in Haskell using
the [Hakyll](https://jaspervdj.be/hakyll) static site generator, and can be easily and reproducibly build with
Stack.

```bash
stack build
```

Once the site generator itself has been built, the site itself can be generated by using the built binary.

```bash
stack exec site build
```

## Deploying

The site itself is hosted on [Surge](https://surge.sh), and can be deployed (with the correct credentials
in place) using the built-in deployment support in Hakyll.

```bash
stack exec site deploy
```

