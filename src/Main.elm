module Main exposing (..)

import Browser as Browser exposing (element)
import Browser.Events exposing (Visibility)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (fromString)
import Color exposing (Color)
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Json.Decode as Decode



-- Type Aliases ----------------------------------------------------------------
-- when the user is moused over the div,
-- the time left before it expires should pause


type alias UIFlags =
    { display_controls : Float -- time left to display controls list
    }


type alias Velocity =
    { x_vel : Float
    , y_vel : Float
    }


type alias BrowserSize =
    { width : Int
    , height : Int
    }


type alias DVD =
    { width : Int
    , height : Int
    }


type alias Flag =
    { windowWidth : Int
    , windowHeight : Int
    , choices : List String
    }


type alias BumpRateLimit =
    { up : Int
    , left : Int
    , down : Int
    , right : Int
    }



-- Event Helpers ----------------------------------------------------------------
-- Key Events
-- the directions a user can move the the DVD logo


type KeyDirection
    = Up
    | Left
    | Down
    | Right



-- actions user can give using the keyboard


type UserInputEvent
    = Movement KeyDirection
    | ShowHelp
    | NoKey


keyDecoder : Decode.Decoder UserInputEvent
keyDecoder =
    Decode.map keyHandler (Decode.field "key" Decode.string)


keyHandler : String -> UserInputEvent
keyHandler inputStr =
    case String.uncons inputStr of
        Just ( char, "" ) ->
            case Char.toLower char of
                'w' ->
                    Movement Up

                'a' ->
                    Movement Left

                's' ->
                    Movement Down

                'd' ->
                    Movement Right

                'h' ->
                    ShowHelp

                _ ->
                    NoKey

        _ ->
            NoKey



-- Visibility Change


isPaused : Model -> Bool
isPaused model =
    case model.active of
        Browser.Events.Visible ->
            True

        Browser.Events.Hidden ->
            False


updateDVDSize : BrowserSize -> DVD
updateDVDSize browser =
    { width = browser.width // 5
    , height = browser.width // 10
    }



-- Model -----------------------------------------------------------------------


type alias Model =
    { x_location : Float
    , y_location : Float --
    , ticks : Float -- ms passed since page load
    , velocity : Velocity -- x,y velocity of dvd
    , flags : UIFlags -- specifies whether the help message is being displayed
    , userInput : UserInputEvent -- user input event to consume
    , active : Visibility -- whether page is focused or not, pause game
    , browserSize : BrowserSize -- the current browser size (width, height)
    , choices : List String -- list of strings to select from for logo text
    , dvd : DVD -- the size of the dvd div itself
    }



-- Init ----------------------------------------------------------------------


initModel : Flag -> Model
initModel flags =
    { x_location = 0.0
    , y_location = 0.0
    , ticks = 0.0
    , velocity = { x_vel = 1.0, y_vel = 1.0 }
    , flags = { display_controls = 10 }
    , userInput = NoKey
    , active = Browser.Events.Visible
    , browserSize = { width = flags.windowWidth, height = flags.windowHeight }
    , choices = flags.choices
    , dvd = updateDVDSize { width = flags.windowWidth, height = flags.windowHeight }
    }


init : Flag -> ( Model, Cmd Msg )
init flags =
    ( initModel flags
    , Cmd.none
    )



-- Update ----------------------------------------------------------------------


type Msg
    = Tick Float -- game tick
    | KeyUp UserInputEvent -- listen for key up events
    | VisibilityChanged Visibility -- pause animation on visibility false
    | UpdateBrowserSize Int Int -- browser size changed, update width/height



-- add the elapsed time (milliseconds) to the tick time


incrementTick : Model -> Float -> Model
incrementTick model elapsed_ms =
    { model | ticks = model.ticks + elapsed_ms }



-- updates the location based on the velocity


updateLocation : Model -> Model
updateLocation model =
    { model
        | x_location = model.x_location + model.velocity.x_vel
        , y_location = model.y_location + model.velocity.y_vel
    }



-- checks the location of the dvd logo based on browser size
-- changes velocity if applicable
-- updates the model and runs events on each render tick


gameLoop : Model -> Float -> ( Model, Cmd Msg )
gameLoop model elapsed_ms =
    ( incrementTick model elapsed_ms
        |> updateLocation
    , Cmd.none
    )



-- updates the model when a message is recieved


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed_ms ->
            gameLoop model elapsed_ms

        KeyUp key ->
            ( { model | userInput = key }
            , Cmd.none
            )

        VisibilityChanged vis ->
            ( { model | active = vis }
            , Cmd.none
            )

        UpdateBrowserSize new_width new_height ->
            ( { model
                | browserSize = { width = new_width, height = new_height }
                , dvd = updateDVDSize { width = new_width, height = new_height }
              }
            , Cmd.none
            )



-- Subscriptions ---------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder) -- get keyup events
        , Browser.Events.onVisibilityChange VisibilityChanged -- get visibility change events
        , Browser.Events.onResize (\w h -> UpdateBrowserSize w h)
        ]
            ++ (if isPaused model then
                    -- get a Tick event based on browser render tick rate
                    -- (requestAnimationFrame js func)
                    [ Browser.Events.onAnimationFrameDelta Tick ]

                else
                    []
               )



-- View ------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [ id "tv-screen" ] [ text (Debug.toString model) ]


main : Program Flag Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
