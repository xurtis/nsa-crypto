-- NSA Crypto App

import Sha exposing (..)
import Data exposing (init, update, subscriptions)
import Display exposing (view)
import Html exposing (programWithFlags)


-- Main Program
main = programWithFlags
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

