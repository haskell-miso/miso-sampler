🍱 miso-sampler 
====================

This project contains a sample [miso](https://github.com/dmjio/miso) application with scripts to 
develop against vanilla GHC and to compile to Web Assembly or JavaScript.

```haskell
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html.Element as H
import           Miso.Html.Event as E
import           Miso.Html.Property as P
import           Miso.Lens
import           Miso.String
import qualified Miso.CSS as CSS
import           Miso.CSS (StyleSheet)
-----------------------------------------------------------------------------
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving (Show, Eq)
-----------------------------------------------------------------------------
main :: IO ()
main = run (startApp app)
-----------------------------------------------------------------------------
app :: App Int Action
app = component 0 updateModel viewModel
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Int Action
updateModel = \case
  AddOne ->
    this += 1
  SubtractOne ->
    this -= 1
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
$ nix develop .#ghcjs --command bash -c "make js"
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop --command bash -c "make serve"
```

### Clean

```bash
$ nix develop --command bash -c "make clean"
```

### CI

Ensure that the Haskell miso [cachix](cachix.org) is being used when building your own projects in CI

```yaml
- name: Install cachix
  uses: cachix/cachix-action@v16
  with:
    name: haskell-miso-cachix
```

### Hosting

To upload and host your project to Github Pages, please see [our Github workflow file](https://github.com/haskell-miso/miso-sampler/blob/main/.github/workflows/main.yml#L38-L56) and the necessary Github actions included.
