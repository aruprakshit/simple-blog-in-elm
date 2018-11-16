module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, input, label, li, p, text, textarea, ul)
import Html.Attributes exposing (cols, name, rows, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Model exposing (..)
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Msg


type Msg
    = CreatedPost (Result Http.Error Post)
    | OnInputChange String String
    | SubmitForm
    | ResetForm FormData
    | GotPosts (Result Http.Error (List Post))



-- Actions


createPost : FormData -> Cmd Msg
createPost model =
    Http.post
        { url = "http://localhost:3000/posts"
        , body = jsonBody (postFormEncoder model)
        , expect = Http.expectJson CreatedPost postDecoder
        }


fetchPosts : Cmd Msg
fetchPosts =
    Http.get
        { url = "http://localhost:3000/posts"
        , expect = Http.expectJson GotPosts postsDecoder
        }



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = [], form = initalFormData }
    , fetchPosts
    )


initalFormData =
    FormData "" ""



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetForm formData ->
            ( { model | form = formData }, Cmd.none )

        OnInputChange targetName targetVal ->
            ( { model | form = updateFormContent targetName targetVal model.form }, Cmd.none )

        SubmitForm ->
            ( model, createPost model.form )

        GotPosts result ->
            case result of
                Ok posts ->
                    ( { model | posts = posts }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CreatedPost result ->
            case result of
                Ok post ->
                    ( { model | posts = post :: model.posts }
                    , Task.perform ResetForm <|
                        Task.succeed initalFormData
                    )

                Err _ ->
                    ( model, Cmd.none )


updateFormContent : String -> String -> FormData -> FormData
updateFormContent name value model =
    if name == "title" then
        { model | title = value }

    else
        { model | content = value }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "height" "200px"
        , style "padding" "20px"
        ]
        [ h1 [] [ text "Create a new post" ]
        , form
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "justify-content" "space-between"
            , style "height" "100%"
            , style "width" "300px"
            , onSubmit SubmitForm
            ]
            [ label []
                [ text "Title:  "
                , input
                    [ name "title"
                    , type_ "text"
                    , value model.form.title
                    , onInput (OnInputChange "title")
                    ]
                    []
                ]
            , label []
                [ text "Content:  "
                , textarea
                    [ name "content"
                    , value model.form.content
                    , rows 5
                    , cols 30
                    , onInput (OnInputChange "content")
                    ]
                    []
                ]
            , input
                [ type_ "submit"
                , value "Submit"
                , style "align-self" "flex-start"
                ]
                []
            ]
        , viewPosts model.posts
        ]


viewPosts : List Post -> Html msg
viewPosts posts =
    ul []
        (List.map
            (\post -> viewPost post)
            posts
        )


viewPost : Post -> Html msg
viewPost post =
    li []
        [ h2 [] [ text post.title ]
        , p [] [ text post.body ]
        ]



-- Encoders


jsonBody : JE.Value -> Http.Body
jsonBody value =
    Http.stringBody "application/json" <|
        JE.encode 0 value


postFormEncoder : FormData -> JE.Value
postFormEncoder model =
    JE.object
        [ ( "title", JE.string model.title )
        , ( "body", JE.string model.content )
        ]



-- Decoders


decodePost : String -> List Post
decodePost json =
    case JD.decodeString postDecoder json of
        Ok post ->
            [ post ]

        Err _ ->
            []


postsDecoder : JD.Decoder (List Post)
postsDecoder =
    JD.list postDecoder


postDecoder : JD.Decoder Post
postDecoder =
    JD.map3 Post
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "body" JD.string)
