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

### Browser mode

For interactive development in the browser via the WASM backend

```bash
$ nix develop .#wasm --command -c 'make repl'
```

Paste the URL in your browser and the REPL will load

```
Preprocessing executable 'app' for app-0.1.0.0...
GHCi, version 9.12.2.20250924: https://www.haskell.org/ghc/  :? for help
Open http://127.0.0.1:8080/main.html or import http://127.0.0.1:8080/main.js to boot ghci
```

```
Loaded GHCi configuration from /Users/dmjio/Desktop/miso-sampler/.ghci
[1 of 2] Compiling Main             ( app/Main.hs, interpreted )
Ok, one module loaded.
>>> main
```

<img width="724" height="607" alt="image" src="https://github.com/user-attachments/assets/b474c48d-3f89-4a97-9d9d-9db8217a02be" />


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
