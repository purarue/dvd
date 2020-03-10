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
import Task exposing (..)



-- Constants ------------------------------------------------------------------
-- how many seconds to display the help for on screen


help_display_time : Float
help_display_time =
    7.5



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


type alias Flag =
    { windowWidth : Int
    , windowHeight : Int
    , choices : List String
    }



-- Event Helpers ----------------------------------------------------------------
-- Key Events


type Key
    = Character Char
    | NoKey


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey str =
    case String.uncons str of
        Just ( char, "" ) ->
            Character char

        _ ->
            NoKey



-- tests if the given key matches the char


isKey : Key -> Char -> Bool
isKey key char =
    case key of
        Character c ->
            if c == char then
                True

            else
                False

        _ ->
            False



-- Visibility Change


isPaused : Model -> Bool
isPaused model =
    case model.active of
        Browser.Events.Visible ->
            True

        Browser.Events.Hidden ->
            False



-- Model -----------------------------------------------------------------------


type alias Model =
    { x_location : Float
    , y_location : Float
    , ticks : Float
    , velocity : Velocity
    , flags : UIFlags
    , userInput : Key
    , active : Visibility
    , browserSize : BrowserSize
    , choices: List String
    }



-- Init ----------------------------------------------------------------------


initModel : Flag -> Model
initModel flags =
    { x_location = 0.0
    , y_location = 0.0
    , ticks = 0.0
    , velocity = { x_vel = 1.0, y_vel = 1.0 }
    , flags = { display_controls = help_display_time }
    , userInput = NoKey
    , active = Browser.Events.Visible
    , browserSize = { width = flags.windowWidth, height = flags.windowHeight }
    , choices = flags.choices
    }


init : Flag -> ( Model, Cmd Msg )
init flags =
    ( initModel flags
    , Cmd.none
    )



-- Update ----------------------------------------------------------------------


type Msg
    = Tick Float -- game tick
    | KeyUp Key -- listen for key up events
    | VisibilityChanged Visibility -- pause animation on visibility false
    | UpdateBrowserSize Int Int -- browser size changed, update width/height




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed_ms ->
            ( { model | ticks = model.ticks + elapsed_ms }
            , Cmd.none
            )

            -- for possible future improvements, does not do anything right now
        KeyUp key ->
            ( { model | userInput = key }
            , Cmd.none
            )

        VisibilityChanged vis ->
            ( { model | active = vis }
            , Cmd.none
            )

        UpdateBrowserSize new_width new_height ->
            ( { model | browserSize = { width = new_width, height = new_height } }
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
