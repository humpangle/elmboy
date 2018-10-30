module Debugger.View exposing (view)

import Debugger.Model exposing (Model)
import Debugger.Msg exposing (Msg)
import Html exposing (Html, div, text)


view : Model -> Html Msg
view model =
    div [] [ text "I'm the debugger, yeah!" ]
