module Elmazing exposing (makeElement)

    import Browser
    import Browser.Events
    import Json.Decode as Decode
    import Html exposing (Html)
    import Html.Attributes as Attr


    type alias Model_ a =
      { a | randomNumbers : List Int }


      makeElement
         : (flags -> (Model_ a, Cmd msg))
           -> (msg -> Model_ a -> (Model_ a, Cmd msg))
      -> (msg, msg, msg)
      -> List (Sub msg)
      -> Program flags (Model_ a) msg
      makeElement init update commonMessages moreSubscriptions =
        Browser.element
            { init = init
                    , view = view
                            , update = update
                                , subscriptions = makeSubscriptions commonMessages moreSubscriptions
                                    }


-- https://github.com/elm/browser/blob/1.0.0/notes/keyboard.md
keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

  makeSubscriptions : (msg, msg, msg) -> List (Sub msg) -> model -> Sub msg
  makeSubscriptions (mouseClick, reset, noop) moreSubscriptions _ =
    Sub.batch <|
        [ Browser.Events.onMouseDown (Decode.succeed mouseClick)
        , Browser.Events.onKeyDown
                (Decode.map (\key -> if key == "Escape" then reset else noop) keyDecoder)
                    ] ++ moreSubscriptions


                    view : Model_ a -> Html msg
                    view model =
                      let
                          styles =
                                [ ("position", "fixed")
                                      , ("top", "50%")
                                            , ("left", "50%")
                                                  , ("transform", "translate(-50%, -50%)")
                                                        ]
                                                            display =
                                                                  Html.text <|
                                                                          "Random Numbers: " ++ Debug.toString (List.reverse model.randomNumbers)
                                                                            in
                                                                              Html.div (List.map (\(k, v) -> Attr.style k v) styles) [display]
