🍱 miso-sampler 
====================

A simple example of using [miso](https://github.com/dmjio/miso) w/ nix integration. This project contains a sample miso application with scripts to develop against vanilla GHC and to compile to Web Assembly.

```haskell
-----------------------------------------------------------------------------
main :: IO ()
main = run (startApp app)
-----------------------------------------------------------------------------
app :: App Model Action
app = (component (Model 0) updateModel viewModel)
  { events = pointerEvents
  , styles = [ Sheet sheet ]
  }
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel = \case
  AddOne -> do
    value += 1
  SubtractOne ->
    value -= 1
  SayHelloWorld ->
    io_ (consoleLog "Hello World!")
-----------------------------------------------------------------------------    
```

> [!TIP] 
> This requires installing [nix](https://nixos.org) with [Nix Flakes](https://wiki.nixos.org/wiki/Flakes) enabled.
> Although not required, we recommend using [miso's binary cache](https://github.com/dmjio/miso?tab=readme-ov-file#binary-cache).

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop --experimental-features nix-command --extra-experimental-features flakes
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Build (JavaScript)

```bash
$ nix develop .#ghcjs
$ build app
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve"
```

### Clean

```bash
$ nix develop --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
