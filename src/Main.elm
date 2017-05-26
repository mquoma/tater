port module Main exposing (..)

import Html exposing (Html, input, span, label, text, div, img, video, source, li, ul, a, button)
import Html.Attributes exposing (src, for, id, width, height, controls, src, type_, class)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as JD


port setTime : Float -> Cmd msg


port consoleLog : String -> Cmd msg



---- MODEL ----


type alias Model =
    { message : String
    , inputText : String
    , selectedTime : Maybe Float
    , currentTime : Float
    , setTime : Maybe Float
    , annotations : Maybe (List Annotation)
    , isAnnotationVisible : Bool
    }


type alias Annotation =
    { timeStamp : Float
    , text : String
    , comments : Maybe (List Comment)
    , highlighted : Bool
    , isCommentVisible : Bool
    }


type alias Comment =
    { authorId : String
    , dateTimeStamp : String
    , text : String
    }


init : ( Model, Cmd Msg )
init =
    ( { message = "test"
      , inputText = ""
      , selectedTime = Nothing
      , currentTime = 0.0
      , setTime = Nothing
      , annotations = Nothing
      , isAnnotationVisible = False
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateTime Float
    | SetTime Float
    | AddAnnotation
    | UpdateAnnotation String
    | SaveAnnotation
    | AddComment Annotation
    | UpdateComment String
    | SaveComment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTime f ->
            ( updateTime model f, Cmd.none )

        SetTime f ->
            ( { model | setTime = Just f }, setTime f )

        AddComment annotation ->
            let
                newAnnotations =
                    model.annotations
                        |> Maybe.withDefault []
                        |> List.map
                            (\a ->
                                if a.timeStamp == annotation.timeStamp then
                                    { a | isCommentVisible = True }
                                else
                                    { a | isCommentVisible = False }
                            )
            in
                ( { model
                    | annotations = Just newAnnotations
                    , selectedTime = Just model.currentTime
                  }
                , Cmd.none
                )

        AddAnnotation ->
            ( { model
                | selectedTime = Just model.currentTime
                , isAnnotationVisible = True
              }
            , Cmd.none
            )

        SaveComment ->
            ( model, Cmd.none )

        SaveAnnotation ->
            let
                currentTime =
                    model.currentTime

                selectedTime =
                    model.selectedTime
                        |> Maybe.withDefault 0

                newAnnotation =
                    Annotation selectedTime model.inputText Nothing False False

                newAnnotations =
                    case model.annotations of
                        Nothing ->
                            [ newAnnotation ]

                        Just a ->
                            List.concat [ a, [ newAnnotation ] ]
            in
                ( { model
                    | annotations = Just newAnnotations
                    , selectedTime = Nothing
                    , inputText = ""
                    , isAnnotationVisible = False
                  }
                , Cmd.none
                )

                SaveComment ->
                    let
                        currentTime =
                            model.currentTime

                        selectedTime =
                            model.selectedTime
                                |> Maybe.withDefault 0

                        newComment =
                            Comment 0 selectedTime model.inputText 

                        newAnnotations =
                            case model.annotations of
                                Nothing ->
                                    [ newAnnotation ]

                                Just a ->
                                    List.concat [ a, [ newAnnotation ] ]
                    in
                        ( { model
                            | annotations = Just newAnnotations
                            , selectedTime = Nothing
                            , inputText = ""
                            , isAnnotationVisible = False
                          }
                        , Cmd.none
                        )

        UpdateAnnotation input ->
            ( { model | inputText = input }, Cmd.none )

        UpdateComment input ->
            ( { model | inputText = input }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateTime : Model -> Float -> Model
updateTime model f =
    let
        newAnnotations =
            model.annotations
                |> Maybe.withDefault []
                |> List.map (\a -> setHighLight a f)
    in
        { model | annotations = Just newAnnotations, currentTime = f }


setHighLight : Annotation -> Float -> Annotation
setHighLight a f =
    if f >= a.timeStamp then
        { a | highlighted = True }
    else
        { a | highlighted = False }


onTimeUpdate : (Float -> a) -> Html.Attribute a
onTimeUpdate msg =
    on "timeupdate" (JD.map msg targetCurrentTime)


targetCurrentTime : JD.Decoder Float
targetCurrentTime =
    JD.at [ "target", "currentTime" ] JD.float



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ video
            [ id "video"
            , width 460
            , height 130
            , controls True
            , onTimeUpdate UpdateTime
            , onClick AddAnnotation
            ]
            [ source
                [ src "SoccerPractice.mp4"
                , type_ "video/mp4"
                ]
                []
            ]
        , renderAnnotations model.annotations
        , renderAnnotationInput model.isAnnotationVisible
        ]


renderAnnotationInput : Bool -> Html Msg
renderAnnotationInput isVisible =
    case isVisible of
        True ->
            div []
                [ label [ for "newAnnotation " ]
                    [ text "New Annotation: " ]
                , input
                    [ id "newAnnotation "
                    , onInput UpdateAnnotation
                    ]
                    []
                , button [ onClick SaveAnnotation ] [ text "save" ]
                ]

        False ->
            div [] []


renderCommentInput : Bool -> Html Msg
renderCommentInput isVisible =
    case isVisible of
        True ->
            div []
                [ label [ for "newComment " ]
                    [ text "New Comment: " ]
                , input
                    [ id "newComment "
                    , onInput UpdateComment
                    ]
                    []
                , button [ onClick SaveComment ] [ text "save" ]
                ]

        False ->
            div [] []


renderAnnotations : Maybe (List Annotation) -> Html Msg
renderAnnotations annotations =
    case annotations of
        Nothing ->
            div [] []

        Just list ->
            list
                |> List.sortBy .timeStamp
                |> List.map
                    (\annotation ->
                        [ annotationLink annotation
                        , span [] [ text annotation.text ]
                        , span [] [ text " - author" ]
                        , renderAnnotationComments annotation
                        , a [ onClick (AddComment annotation) ] [ text "add comment" ]
                        , renderCommentInput annotation.isCommentVisible
                        ]
                            |> li []
                    )
                |> ul []


renderAnnotationComments : Annotation -> Html Msg
renderAnnotationComments annotation =
    case annotation.comments of
        Nothing ->
            div [] []

        Just list ->
            list
                |> List.sortBy .dateTimeStamp
                |> List.map (\c -> li [] [ text c.text ])
                |> ul []


annotationLink : Annotation -> Html Msg
annotationLink annotation =
    let
        highlighted =
            if annotation.highlighted then
                "annotation-highlighted"
            else
                "annotation"
    in
        a
            [ class highlighted
            , SetTime annotation.timeStamp |> onClick
            ]
            [ toString annotation.timeStamp |> text ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
