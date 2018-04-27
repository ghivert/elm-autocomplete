module Demo exposing (..)

import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Autocomplete

type Msg
  = Autocomplete (Autocomplete.Msg Quotes)
  | SelectElement Quote

type alias Quote =
  { iconUrl : String
  , id : String
  , url : String
  , value : String
  }

type alias Quotes =
  List Quote

type alias Model =
  { autocompleteState : Autocomplete.State Quote Msg
  , quote : Maybe Quote
  }

decodeQuotes : Decoder Quotes
decodeQuotes =
  Decode.at [ "result" ] <|
    Decode.list decodeQuote

decodeQuote : Decoder Quote
decodeQuote =
  Decode.map4 Quote
    (Decode.field "icon_url" Decode.string)
    (Decode.field "id" Decode.string)
    (Decode.field "url" Decode.string)
    (Decode.field "value" Decode.string)

asAutocompleteState : Model -> Autocomplete.State Quote Msg -> Model
asAutocompleteState model autocompleteState =
  { model | autocompleteState = autocompleteState }

init : (Model, Cmd Msg)
init =
  { autocompleteState =
    (Autocomplete.autocompleteState
      Autocomplete
      SelectElement
      [ Autocomplete.get "https://api.chucknorris.io/jokes/search?query=" decodeQuotes ]
    )
      |> Autocomplete.minChars 5
      |> Autocomplete.maxResults 25
  , quote = Nothing
  }
    ! []

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ autocompleteState } as model) =
  case msg of
    Autocomplete msg_ ->
      let
        ( state, cmds ) =
          Autocomplete.update msg_ autocompleteState
      in
        (state |> asAutocompleteState model) ! [ cmds ]
    SelectElement quote ->
      { model | quote = Just quote } ! []

subscriptions : Model -> Sub Msg
subscriptions { autocompleteState } =
  Sub.batch [ Autocomplete.subscriptions autocompleteState 250 ]

cellView : Quote -> Html Msg
cellView { value } =
  Html.text value

view : Model -> Html Msg
view { autocompleteState, quote } =
  Html.div []
    [ Html.h1
      [ Html.Attributes.style [ ("text-align", "center") ] ]
      [ Html.text "Elm Autocomplete of the Chuck Norris Api" ]
    , Html.div
      [ Html.Attributes.style
        [ ("max-width", "700px")
        , ("margin", "auto")
        , ("padding", "12px")
        ]
      ]
      [ Autocomplete.defaultAttributes
        |> Autocomplete.withPlaceholder "Type some Chuck Norris related terms like 'plane' or 'truck'"
        |> Autocomplete.withHover [ ("background-color", "grey") ]
        |> Autocomplete.input autocompleteState cellView
      , case quote of
        Nothing ->
          Html.text ""
        Just { value } ->
          Html.div []
            [ Html.text value ]
      ]
    ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
