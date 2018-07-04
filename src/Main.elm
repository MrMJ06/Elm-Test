module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Html.Events exposing (onInput)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }

type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalVisibility : Modal.Visibility
    }


type Page
    = Home
    | Button
    | List
    | NotFound


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate location { navState = navState, page = Home, modalVisibility= Modal.hidden }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden } 
            , Cmd.none 
            )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown } 
            , Cmd.none 
            )



urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Button (UrlParser.s "buttonPage")
        , UrlParser.map List (UrlParser.s "list")
        ]


view : Model -> Html Msg
view model =
    div []
        [ menu model
        , mainContent model
        , modal model
        ]


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Main view" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#buttonPage" ] [ text "Login" ]
            , Navbar.itemLink [ href "#list" ] [ text "List" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            Button ->
                pageGettingStarted model

            List ->
                pageModules model

            NotFound ->
                pageNotFound


pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "Home" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Login" ]
                |> Card.block []
                    [ Block.text [] [ text "Click the login button." ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#buttonPage" ] ]
                            [ text "Login" ]
                    ]
                |> Card.view
            ]
        , Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.headerH4 [] [ text "List" ]
                |> Card.block []
                    [ Block.text [] [ text "A list example" ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#list" ] ]
                            [ text "List" ]
                    ]
                |> Card.view
            ]
        ]
    ]


pageGettingStarted : Model -> List (Html Msg)
pageGettingStarted model =
    [ h2 [] [ text "Login" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick ShowModal ]
        ]
        [ text "Login" ]
    ]


pageModules : Model -> List (Html Msg)
pageModules model =
    [ h1 [] [ text "List test" ]
    , Listgroup.ul
        [ Listgroup.li [] [ text "Elm 1" ]
        , Listgroup.li [] [ text "Elm 2" ]
        , Listgroup.li [] [ text "Elm 2" ]
        ]
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]

type alias Number = Int
number : Number
number = 0
type Txt
    = Name String
    | Password String
    | PasswordAgain String


modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h4 [] [ text "Login" ]
        |> Modal.body []
            [ InputGroup.config
            (InputGroup.text [ Input.placeholder "username"])
            |> InputGroup.predecessors
                [ InputGroup.span [] [ text ""] ]
            |> InputGroup.view
                , br [] []
                , InputGroup.config
                    (InputGroup.text [ Input.placeholder "password"])
                    |> InputGroup.predecessors
                    [ InputGroup.span [] [ text "*"] ]
                    |> InputGroup.view
                , br [] []
                , InputGroup.config
                    (InputGroup.text [ Input.placeholder "confirmPassword"])
                    |> InputGroup.predecessors
                        [ InputGroup.span [] [ text "*"] ]
                    |> InputGroup.view
                , br [] []
                ]
             
        |> Modal.view model.modalVisibility
