module Main exposing (..)

import Browser as Browser exposing (element)
import Browser.Events exposing (Visibility(..))
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List exposing (head, map)
import Maybe exposing (withDefault)



-- Constants -------------------------------------------------------------------


{-| default amount of pixels to move each animation frame
-}
default_velocity : Int
default_velocity =
    3


{-| default time to display help modeal for
-}
default_display_time : Float
default_display_time =
    5000.0


{-| rate limit duration
-}
default_rate_limit : Float
default_rate_limit =
    3000.0


{-| distance to bump the logo
-}
default_bump : Int
default_bump =
    5



-- Type Aliases ----------------------------------------------------------------


{-| when the mouse is moving, ,
the time left before it expires should pause
-}
type alias UIFlags =
    { display_help : Float -- ms left to display controls list
    , userClosedModal : Bool
    }


type alias Velocity =
    { x : Int
    , y : Int
    }


type alias Coordinate =
    { x : Int
    , y : Int
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
    , debug : Bool
    }



-- Event Helpers ----------------------------------------------------------------
-- Key Events


{-| the directions a user can move the the DVD logo
-}
type KeyDirection
    = K_Up
    | K_Left
    | K_Down
    | K_Right


type VelocityChange
    = SpeedUp
    | SlowDown


{-| actions user can give using the keyboard
-}
type UserInputEvent
    = Movement KeyDirection
    | Speed VelocityChange
    | NoKey


{-| Handle decoding the X,Y position of the mouse on the page
-}
mouseDecoder : Decode.Decoder Coordinate
mouseDecoder =
    Decode.map2 Coordinate
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)


keyDecoder : Decode.Decoder UserInputEvent
keyDecoder =
    Decode.map keyHandler (Decode.field "key" Decode.string)


keyHandler : String -> UserInputEvent
keyHandler inputStr =
    case String.uncons inputStr of
        Just ( char, "" ) ->
            case Char.toLower char of
                'w' ->
                    Movement K_Up

                'a' ->
                    Movement K_Left

                's' ->
                    Movement K_Down

                'd' ->
                    Movement K_Right

                'q' ->
                    Speed SlowDown

                'e' ->
                    Speed SpeedUp

                _ ->
                    NoKey

        _ ->
            NoKey


{-| Return Boolean that describes whether or not animation is paused
-}
isPaused : Visibility -> Bool
isPaused vis =
    case vis of
        Visible ->
            True

        Hidden ->
            False


updateDVDSize : BrowserSize -> DVD
updateDVDSize browser =
    { width = browser.width // 5
    , height = browser.width // 10
    }



-- Model -----------------------------------------------------------------------


type alias Model =
    { location : Coordinate -- x, y location of the dvd div
    , ticks : Float -- ms passed since page load
    , velocity : Velocity -- x,y velocity of dvd
    , flags : UIFlags -- specifies whether the help message is being displayed
    , active : Visibility -- whether page is focused or not, pause game
    , browserSize : BrowserSize -- the current browser size (width, height)
    , choices : List String -- list of strings to select from for logo text
    , chosen : String -- current text to display
    , dvd : DVD -- the size of the dvd div itself
    , score : Int -- numbers of times the dvd has hit the corner
    , debug : Bool -- display the model as the page instead of the animation
    , mouseLocation : Coordinate
    , rateLimit : Float -- the 3s wait limit for bumping the logo
    }



-- Init ----------------------------------------------------------------------


initModel : Flag -> Model
initModel flags =
    let
        browserSize =
            { width = flags.windowWidth, height = flags.windowHeight }
    in
    { location = { x = browserSize.width // 5, y = browserSize.height // 5 }
    , ticks = 0.0
    , velocity = { x = default_velocity, y = default_velocity }
    , flags = { display_help = default_display_time, userClosedModal = False }
    , active = Visible
    , browserSize = browserSize
    , choices = flags.choices
    , chosen = head flags.choices |> withDefault "DVD"
    , dvd = updateDVDSize browserSize
    , score = 0
    , debug = flags.debug
    , mouseLocation = { x = 0, y = 0 }
    , rateLimit = 0.0
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
    | IncrementScore -- called from the updateVelocity function, to signify user hit the corner
    | MouseMove Coordinate -- listen for when the mouse moves
    | UserClosedHelp -- dont show help on mouse move if user clicks 'x'



-- methods to manage colliding with the edge of the screen
-- get x, y location for corners of the dvd div


type CornerType
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


getDVDCornerLocation : Model -> CornerType -> Coordinate
getDVDCornerLocation model corner =
    case corner of
        TopLeft ->
            { x = model.location.x
            , y = model.location.y
            }

        TopRight ->
            { x = model.location.x + model.dvd.width
            , y = model.location.y
            }

        BottomLeft ->
            { x = model.location.x
            , y = model.location.y + model.dvd.height
            }

        BottomRight ->
            { x = model.location.x + model.dvd.width
            , y = model.location.y + model.dvd.height
            }


{-| check if the div hit a corner or edge, update
velocity and score accordingly
-}
updateVelocity : Model -> Model
updateVelocity model =
    -- check corners and then edges
    let
        topLeftCoords =
            getDVDCornerLocation model TopLeft

        topRightCoords =
            getDVDCornerLocation model TopRight

        bottomLeftCoords =
            getDVDCornerLocation model BottomLeft

        bottomRightCoords =
            getDVDCornerLocation model BottomRight

        incrementScore : Model -> Model
        incrementScore mdl =
            let
                ( new_model, _ ) =
                    update IncrementScore mdl
            in
            new_model

        -- flip horizontal velocity
        flipVelocityHorizontal : Model -> Model
        flipVelocityHorizontal fh_model =
            let
                vel =
                    fh_model.velocity
            in
            { fh_model
                | velocity = { vel | x = -vel.x }
            }

        -- flip vertical velocity
        flipVelocityVertical : Model -> Model
        flipVelocityVertical fv_model =
            let
                vel =
                    model.velocity
            in
            { fv_model
                | velocity = { vel | y = -vel.y }
            }

        -- flip velocity for both directions (for corners)
        flipVelocityBoth : Model -> Model
        flipVelocityBoth fb_model =
            let
                vel =
                    fb_model.velocity
            in
            { fb_model
                | velocity =
                    { vel
                        | x = -vel.x
                        , y = -vel.y
                    }
            }

        -- incase the div clips out of bounds
        -- due to user input, reset it
        resetOutOfBounds : Model -> Model
        resetOutOfBounds r_model =
            let
                l =
                    r_model.location
            in
            -- top of screen
            if topLeftCoords.y < 0 then
                { r_model | location = { l | y = l.y - topLeftCoords.y } }
                -- left of screen

            else if topLeftCoords.x < 0 then
                { r_model | location = { l | x = l.x - topLeftCoords.x } }
                -- right of screen

            else if bottomRightCoords.x > model.browserSize.width then
                { r_model | location = { l | x = model.browserSize.width - model.dvd.width } }
                -- bottom of screen

            else if bottomRightCoords.y > model.browserSize.height then
                { r_model | location = { l | y = model.browserSize.height - model.dvd.height } }

            else
                r_model
    in
    if
        -- flip both velocities ( hit the corner )
        (topLeftCoords.x <= 0 && topLeftCoords.y <= 0)
            || (topRightCoords.x >= model.browserSize.width && topRightCoords.y <= 0)
            || (bottomLeftCoords.x <= 0 && bottomLeftCoords.y >= model.browserSize.height)
            || (bottomRightCoords.x >= model.browserSize.width && bottomRightCoords.y >= model.browserSize.height)
    then
        model |> flipVelocityBoth |> resetOutOfBounds |> incrementScore
        -- vertical flip velocity

    else if topLeftCoords.y <= 0 || bottomLeftCoords.y >= model.browserSize.height then
        model |> flipVelocityVertical |> resetOutOfBounds
        -- horizontal flip velocity

    else if topLeftCoords.x <= 0 || topRightCoords.x >= model.browserSize.width then
        model |> flipVelocityHorizontal |> resetOutOfBounds

    else
        model


{-| checks the location of the dvd logo based on browser size
changes velocity if applicable
updates the model and runs events on each render tick
-}
gameLoop : Model -> Float -> ( Model, Cmd Msg )
gameLoop model elapsed_ms =
    let
        -- add the elapsed time (milliseconds) to the tick time
        incrementTick : Model -> Float -> Model
        incrementTick t_model new_ticks =
            { t_model | ticks = t_model.ticks + new_ticks }

        -- decay time to display the help message
        decayHelpTime : Float -> Model -> Model
        decayHelpTime decay_ms d_model =
            if d_model.flags.display_help > 0 then
                let
                    m_flags =
                        d_model.flags
                in
                { d_model | flags = { m_flags | display_help = m_flags.display_help - decay_ms } }

            else
                d_model

        -- decay time for bumping logo
        decayRateLimit : Float -> Model -> Model
        decayRateLimit decay_ms d_model =
            if d_model.rateLimit > 0 then
                { d_model | rateLimit = d_model.rateLimit - decay_ms }

            else
                d_model

        -- updates the location based on the velocity
        updateLocation : Model -> Model
        updateLocation u_model =
            { u_model
                | location =
                    { x = u_model.location.x + u_model.velocity.x
                    , y = u_model.location.y + u_model.velocity.y
                    }
            }
    in
    ( incrementTick model elapsed_ms
        |> decayHelpTime elapsed_ms
        |> decayRateLimit elapsed_ms
        |> updateVelocity
        |> updateLocation
    , Cmd.none
    )


{-| Handle's the User Input for modifying location/speed
for the logo

Moving the logo resets the rate limit to prevent spamming
the movement commands

-}
consumeUserInputEvent : Model -> UserInputEvent -> Model
consumeUserInputEvent model ui_event =
    case ui_event of
        Movement dir ->
            let
                l =
                    model.location

                -- sets the rate limit to the default
                -- only called when the div was moved
                setRateLimit : Model -> Model
                setRateLimit s_model =
                    { s_model | rateLimit = default_rate_limit }

                -- executes the passed function if the model
                -- passed the rate limit
                ifRateRun : Model -> (Model -> Model) -> Model
                ifRateRun p_model modify_model =
                    if p_model.rateLimit <= 0 then
                        modify_model p_model |> setRateLimit

                    else
                        model
            in
            case dir of
                K_Up ->
                    ifRateRun model (\m -> { m | location = { l | y = l.y - default_bump } })

                K_Left ->
                    ifRateRun model (\m -> { m | location = { l | x = l.x - default_bump } })

                K_Right ->
                    ifRateRun model (\m -> { m | location = { l | x = l.x + default_bump } })

                K_Down ->
                    ifRateRun model (\m -> { m | location = { l | y = l.y + default_bump } })

        Speed change ->
            let
                -- makes sure that velocity is within some sane range; 2 - ( 2 * default_velocity )
                verifyVelocity : Model -> Int -> Bool
                verifyVelocity v_model proposed_change =
                    let
                        magnitude =
                            abs v_model.velocity.x + proposed_change
                    in
                    magnitude >= 2 && magnitude <= 2 * default_velocity

                -- increases/decreases the velocity
                modifyVelocity : Int -> Model -> Model
                modifyVelocity change_vel v_model =
                    let
                        vel =
                            v_model.velocity

                        -- incrase/decrease positive/negative velocities
                        apply_speed : Int -> Int -> Int
                        apply_speed old_vel inc_dec =
                            let
                                new_magnitude =
                                    abs old_vel + inc_dec
                            in
                            if old_vel > 0 then
                                new_magnitude

                            else
                                -new_magnitude
                    in
                    { v_model
                        | velocity =
                            { x = apply_speed vel.x change_vel
                            , y = apply_speed vel.y change_vel
                            }
                    }
            in
            case change of
                SpeedUp ->
                    if verifyVelocity model 1 then
                        modifyVelocity 1 model

                    else
                        model

                SlowDown ->
                    if verifyVelocity model -1 then
                        modifyVelocity -1 model

                    else
                        model

        NoKey ->
            model


{-| updates the model when a message is recieved
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- handle UpdateBrowserSize
        updateOnBrowserResize : Model -> Int -> Int -> Model
        updateOnBrowserResize u_model new_width new_height =
            let
                browserSize =
                    { width = new_width, height = new_height }
            in
            { u_model
                | dvd = updateDVDSize browserSize
                , browserSize = browserSize
            }
    in
    case msg of
        Tick elapsed_ms ->
            gameLoop model elapsed_ms

        KeyUp key ->
            ( consumeUserInputEvent model key
            , Cmd.none
            )

        VisibilityChanged vis ->
            ( { model | active = vis }
            , Cmd.none
            )

        UpdateBrowserSize new_width new_height ->
            ( updateOnBrowserResize model new_width new_height
            , Cmd.none
            )

        IncrementScore ->
            ( { model | score = model.score + 1 }
            , Cmd.none
            )

        -- ignore the coordinate, just capture the mousemove event
        MouseMove _ ->
            -- whenever the mouse moves, reset the display time for the modal
            let
                handleMouseMove : Model -> Model
                handleMouseMove h_model =
                    let
                        m_flags =
                            h_model.flags
                    in
                    { h_model | flags = { m_flags | display_help = default_display_time } }
            in
            ( model |> handleMouseMove
            , Cmd.none
            )

        UserClosedHelp ->
            let
                ui_flags =
                    model.flags
            in
            ( { model | flags = { ui_flags | userClosedModal = True } }
            , Cmd.none
            )



-- Subscriptions ---------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder) -- get keyup events
        , Browser.Events.onVisibilityChange VisibilityChanged -- get visibility change events
        , Browser.Events.onResize (\w h -> UpdateBrowserSize w h)
        , Browser.Events.onMouseMove (Decode.map MouseMove mouseDecoder)
        ]
            ++ (if isPaused model.active then
                    -- get a Tick event based on browser render tick rate
                    -- (requestAnimationFrame js func)
                    [ Browser.Events.onAnimationFrameDelta Tick ]

                else
                    []
               )



-- View ------------------------------------------------------------------------


renderHelp : Model -> Html Msg
renderHelp model =
    if not model.flags.userClosedModal && model.flags.display_help > 0 then
        div
            [ id "help" ]
            ([ div
                [ id "close-button"
                , onClick UserClosedHelp
                ]
                [ text "×" ]
             ]
                ++ map (\ds -> div [] [ text ds ]) [ "• WASD to bump", "logo every 3/sec", "• Q/E to adjust speed" ]
            )

    else
        div [] []


renderDVD : Model -> Html Msg
renderDVD model =
    let
        pixel : Int -> String
        pixel pixelInt =
            String.fromInt pixelInt ++ "px"
    in
    div
        [ style "width" (pixel model.dvd.width)
        , style "height" (pixel model.dvd.height)
        , style "left" (pixel model.location.x)
        , style "top" (pixel model.location.y)
        , id "dvd"
        ]
        [ span
            []
            [ text model.chosen ]
        ]


debugInfo : Model -> Html msg
debugInfo model =
    if model.debug then
        div [ id "debug" ] [ text (Debug.toString model) ]

    else
        div [] []


view : Model -> Html Msg
view model =
    div
        [ id "tv-screen" ]
        [ renderHelp model
        , renderDVD model
        , debugInfo model
        ]


main : Program Flag Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
