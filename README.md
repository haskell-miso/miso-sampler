🍱 miso-sampler 
====================

This project contains a sample [miso](https://github.com/dmjio/miso) application with scripts to develop against vanilla GHC and to compile to Web Assembly or JavaScript.

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
newtype Model = Model { _value :: Int }
  deriving (Show, Eq)
-----------------------------------------------------------------------------
value :: Lens Model Int
value = lens _value $ \m v -> m { _value = v }
-----------------------------------------------------------------------------
data Action
  = AddOne PointerEvent
  | SubtractOne PointerEvent
  | SayHelloWorld
  deriving (Show, Eq)
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startApp app
-----------------------------------------------------------------------------
app :: App Model Action
app = (component (Model 0) updateModel viewModel)
  { events = pointerEvents
  , styles = [ Sheet sheet ]
  }
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel = \case
  AddOne event ->
    value += 1
  SubtractOne event ->
    value -= 1
  SayHelloWorld ->
    io_ (consoleLog "Hello World!")
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel x = H.div_
  [ P.class_ "counter-container" ]
  [ H.h1_
    [ P.class_ "counter-title"
    ]
    [ "🍜 Miso sampler"
    ]
  , H.div_
    [ P.class_ "counter-display"
    ]
    [ text (ms x)
    ]
  , H.div_
    [ P.class_ "buttons-container"
    ]
    [ H.button_
      [ E.onPointerDown AddOne
      , P.class_ "decrement-btn"
      ] [text "+"]
    , H.button_
      [ E.onPointerDown SubtractOne
      , P.class_ "increment-btn"
      ] [text "-"]
    ]
  ]   
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
$ nix develop .#wasm --command bash -c "make serve"
```

### Clean

```bash
$ nix develop --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
