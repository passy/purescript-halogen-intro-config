module Main where

import Data.Void
import Data.Tuple
import Data.Either

import Control.Bind
import Control.Monad.Eff

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- | The state of the application
newtype State = State { on :: Boolean }

-- | Global configuration
newtype Config = Config { toggleText :: String
                        , onText :: String
                        , offText :: String
                        }

-- | Inputs to the state machine
data Input = ToggleState

ui :: forall m r eff. (Monad m) => Reader Config (Component m Input Input)
ui = do
  conf <- ask
  return $ render conf <$> stateful (State { on: false }) update
  where
    render :: Config -> State -> H.HTML (m Input)
    render (Config conf) (State s) =
      H.div_ [ H.h1_ [ H.text conf.toggleText ]
             , H.button [ A.onClick (A.input_ ToggleState) ]
                        [ H.text (if s.on then conf.onText else conf.offText) ]
             ]

    update :: State -> Input -> State
    update (State s) ToggleState = State { on: not s.on }

main = do
  let config = Config { toggleText: "Toggle Button"
                      , onText: "On"
                      , offText: "Off"
                      }
  Tuple node _ <- runUI $ runReader ui config
  appendToBody node
