port module Main exposing (main)
import Browser
import Html exposing (Html)
import Html.Events as Ev
import Html.Attributes as Attr
import Time exposing (Posix)
import Json.Decode as D
import Json.Encode as E
import BigInt exposing (..)
import Array exposing (Array)
import UpgradeData exposing (..)

-- MAIN

main : Program E.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = \_ -> Time.every 100 Tick
    }


-- MODEL

zero : BigInt
zero =
  fromInt 0

get n arr =
  Array.get n arr
    |> Maybe.withDefault 0

type alias Model =
  { iorin : BigInt
  , deltaIorin : BigInt
  , upgrade : Array Int
  }

init : E.Value -> (Model, Cmd Msg)
init flags =
  let
    initModel =
      case D.decodeValue decoder flags of
        Ok model -> model
        Err _ ->
          { iorin = zero
          , deltaIorin = zero
          , upgrade = Array.repeat 4 0
          }
  in
    (initModel, Cmd.none)

-- UPDATE

type Msg
  = Pressed Int
  | Tick Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pressed n ->
      let
        upgradeLevel =
          get n model.upgrade

        current =
          sub model.iorin (cost n upgradeLevel)

      in
        if gte current zero
        then
          let
            (newDelta, updatedArr) = updateDelta n (upgradeLevel+1) model.deltaIorin model.upgrade
            (newIorin, newArr) = updateIorin n (upgradeLevel+1) current updatedArr
          in
            ( Model newIorin newDelta (Array.set n (upgradeLevel+1) newArr)
            , Cmd.none
            )
        else
          (model, Cmd.none)


    Tick _ ->
      ( { model
        | iorin = add model.iorin model.deltaIorin
        }
      , Cmd.none
      )

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.text <| toString model.iorin ++ " Iorins"
    , Html.br [][]
    , Html.text <| toString model.deltaIorin ++ " deltaIorin"
    , viewButtons model.upgrade
    ]


viewButtons arr =
  List.range 0 4
    |> List.map
        (\n ->
            Html.div []
              [ Html.button
                [ Ev.onClick <| Pressed n]
                [ Html.text
                    <| description n (get n arr)
                ]
              ]
        )
    |> Html.div []



-- PORTS

port setLocalStorage : E.Value -> Cmd msg

updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg oldModel =
  let
    (newModel, cmd) = update msg oldModel
  in
    ( newModel
    , Cmd.batch
        [ setLocalStorage (encode newModel)
        , cmd
        ]
    )



-- JSON ENCODE/DECODE

encode : Model -> E.Value
encode model =
  E.object
    [ ("iorin", E.string <| toString model.iorin)
    , ("deltaIorin", E.string <| toString model.deltaIorin)
    , ("upgrade", (E.array E.int) model.upgrade )
    ]

fromStr str =
  fromIntString str
    |> Maybe.withDefault (zero)

decoder : D.Decoder Model
decoder =
  D.map3 Model
    (D.field "iorin" <| D.map fromStr D.string)
    (D.field "deltaIorin" <| D.map fromStr D.string )
    (D.field "upgrade" (D.array D.int ))