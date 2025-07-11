miso-sampler
====================

A simple example of using [miso](https://github.com/dmjio/miso) w/ nix integration.

This serves as a good template for getting started, we might wrap all of this in a 
`create-miso-app` project.

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop
```

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
