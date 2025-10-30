-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
instance ToMisoString Model where
  toMisoString (Model v) = toMisoString v
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
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
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
  AddOne event -> do
    value += 1
    io_ $ consoleLog (ms (show event))
  SubtractOne event -> do
    value -= 1
    io_ $ consoleLog (ms (show event))
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
-----------------------------------------------------------------------------
sheet :: StyleSheet
sheet =
  CSS.sheet_
  [ CSS.selector_ ":root"
    [ "--primary-color" =: "#4a6bff"
    , "--primary-hover" =: "#3451d1"
    , "--secondary-color" =: "#ff4a6b"
    , "--secondary-hover" =: "#d13451"
    , "--background" =: "#f7f9fc"
    , "--text-color" =: "#333"
    , "--shadow" =: "0 4px 10px rgba(0, 0, 0, 0.1);"
    , "--transition" =: "all 0.3s ease;"
    ]
  , CSS.selector_ "body"
    [ CSS.fontFamily "'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"
    , CSS.display "flex"
    , CSS.justifyContent "center"
    , CSS.alignItems "center"
    , CSS.height "100vh"
    , CSS.margin "0"
    , CSS.backgroundColor (CSS.var "background")
    , CSS.color (CSS.var "text-color")
    ]
  , CSS.selector_ ".counter-container"
    [ CSS.backgroundColor CSS.white
    , CSS.padding (CSS.rem 2)
    , CSS.borderRadius (CSS.px 12)
    , CSS.boxShadow "shadow"
    , CSS.textAlign "center"
    ]
  , CSS.selector_ ".counter-display"
    [ CSS.fontSize "5rem"
    , CSS.fontWeight "bold"
    , CSS.margin "1CSS.rem 0"
    , CSS.transition "var(--transition)"
    ]
  , CSS.selector_ ".buttons-container"
    [ CSS.display "flex"
    , CSS.gap "1rem"
    , CSS.justifyContent "center"
    , CSS.marginTop "1.5rem"
    ]
  , CSS.selector_ "button"
    [ CSS.fontSize "1.5rem"
    , CSS.width "3rem"
    , CSS.height "3rem"
    , CSS.border "none"
    , CSS.borderRadius "50%"
    , CSS.cursor "pointer"
    , CSS.transition "var(--transition)"
    , CSS.color CSS.white
    , CSS.display "flex"
    , CSS.alignItems "center"
    , CSS.justifyContent "center"
    ]
  , CSS.selector_ ".increment-btn"
    [ CSS.backgroundColor (CSS.var "primary-color")
    ]
  , CSS.selector_ ".increment-btn:hover"
    [ CSS.backgroundColor (CSS.var "primary-hover")
    , CSS.transform "translateY(-2px)"
    ]
  , CSS.selector_ ".decrement-btn"
    [ CSS.backgroundColor (CSS.var "secondary-color")
    ]
  , CSS.selector_ ".decrement-btn:hover"
    [ CSS.backgroundColor (CSS.var "secondary-hover")
    , CSS.transform "translateY(-2px)"
    ]
  , CSS.keyframes_ "pulse"
    [ CSS.pct 0 =:
      [ CSS.transform "scale(1)"
      ]
    , CSS.pct 50 =:
      [ CSS.transform "scale(1.1)"
      ]
    , CSS.pct 100 =:
      [ CSS.transform "scale(1)"
      ]
    ]
  , CSS.selector_ ".counter-display.animate"
    [ CSS.animation "pulse 0.3s ease"
    ]
  , CSS.media_ "(max-width: 480px)"
    [ ".counter-container" =:
      [ CSS.padding (CSS.rem 1.5)
      ]
    , ".counter-display" =:
      [ CSS.fontSize (CSS.rem 3)
      ]
    , "button" =:
      [ CSS.fontSize (CSS.rem 1.2)
      , CSS.width (CSS.rem 2.5)
      , CSS.width (CSS.rem 2.5)
      ]
    ]
  ]
-----------------------------------------------------------------------------
