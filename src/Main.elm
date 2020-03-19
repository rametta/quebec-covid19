module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, oneOf, parse)


type Lang
    = En
    | Fr
    | None


type Answer
    = Yes
    | No
    | NotAnswered


type Page
    = Home
    | EnQuestion Int
    | FrQuestion Int
    | EnResults
    | FrResults


type Msg
    = UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | SetAnswers Answers


type alias Answers =
    { q1 : Answer
    , q2 : Answer
    , q3 : Answer
    , q4 : Answer
    , q5 : Answer
    , q6 : Answer
    }


type alias Model =
    { navKey : Nav.Key
    , navUrl : Url
    , page : Page
    , lang : Lang
    , answers : Answers
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        page =
            urlToPage url
    in
    ( { navKey = key
      , navUrl = url
      , page = page
      , lang = pageLang page
      , answers =
            { q1 = NotAnswered
            , q2 = NotAnswered
            , q3 = NotAnswered
            , q4 = NotAnswered
            , q5 = NotAnswered
            , q6 = NotAnswered
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            let
                page =
                    urlToPage url
            in
            ( { model | navUrl = url, page = page, lang = pageLang page }, Cmd.none )

        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        SetAnswers answers ->
            ( { model | answers = answers }, Cmd.none )


routeParser : Parser (Page -> a) a
routeParser =
    let
        s =
            Url.Parser.s

        map =
            Url.Parser.map
    in
    oneOf
        [ map Home (s "")
        , map EnQuestion (s "en" </> Url.Parser.int)
        , map FrQuestion (s "fr" </> Url.Parser.int)
        , map EnResults (s "en" </> s "results")
        , map FrResults (s "fr" </> s "resultats")
        ]


urlToPage : Url -> Page
urlToPage =
    parse routeParser >> Maybe.withDefault Home


pageLang : Page -> Lang
pageLang page =
    case page of
        Home ->
            None

        EnQuestion _ ->
            En

        FrQuestion _ ->
            Fr

        EnResults ->
            En

        FrResults ->
            Fr


viewNav : Lang -> Html Msg
viewNav lang =
    nav [ class "navbar qc-nav has-text-white is-flex", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item has-text-white", href "/" ]
                [ text "Quebec | Self Assessment"
                ]
            ]
        , div [ class "buttons has-addons mr-1" ]
            [ a [ class "button is-small", classList [ ( "is-link is-selected", lang == En ) ], href "/en/1", attribute "aria-label" "English" ] [ text "EN" ]
            , a [ class "button is-small", classList [ ( "is-link is-selected", lang == Fr ) ], href "/fr/1", attribute "aria-label" "Francais" ] [ text "FR" ]
            ]
        ]


viewHome : Html Msg
viewHome =
    div [ class "section" ]
        [ div [ class "buttons has-addons are-large" ]
            [ a [ class "button", href "/en/1" ] [ text "English" ]
            , a [ class "button", href "/fr/1" ] [ text "Français" ]
            ]
        ]


viewStep : Int -> Html Msg
viewStep step =
    progress
        [ class "progress is-link steps-bar is-small is-marginless"
        , value <| String.fromInt step
        , Html.Attributes.max "6"
        ]
        []


viewEnQ1 : Answers -> Html Msg
viewEnQ1 answers =
    div []
        [ viewStep 1
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Are you experiencing any of the following:"
                    , ul []
                        [ li [] [ text "Severe difficulty breathing (e.g. struggling to breathe or speaking in single words)" ]
                        , li [] [ text "Severe chest pain" ]
                        , li [] [ text "Having a very hard time waking up" ]
                        , li [] [ text "Feeling confused" ]
                        , li [] [ text "Losing consciousness" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "buttons is-centered has-addons are-medium" ]
                        [ button
                            [ onClick (SetAnswers { answers | q1 = Yes })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q1 == Yes ) ]
                            ]
                            [ text "Yes" ]
                        , button
                            [ onClick (SetAnswers { answers | q1 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q1 == No ) ]
                            ]
                            [ text "No" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/" ] [ text "Home" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "1/6" ]
                        , case answers.q1 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "Next" ]

                            _ ->
                                a [ class "button is-link", href "/en/2" ] [ text "Next" ]
                        ]
                    ]
                ]
            ]
        ]


viewEnQ2 : Answers -> Html Msg
viewEnQ2 answers =
    div []
        [ viewStep 2
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Are you experiencing any of the following:"
                    , ul []
                        [ li [] [ text "Shortness of breath at rest" ]
                        , li [] [ text "Inability to lie down because of difficulty breathing" ]
                        , li [] [ text "Chronic health conditions that you are having difficulty managing because of difficulty breathing" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "buttons is-centered has-addons are-medium" ]
                        [ button
                            [ onClick (SetAnswers { answers | q2 = Yes })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q2 == Yes ) ]
                            ]
                            [ text "Yes" ]
                        , button
                            [ onClick (SetAnswers { answers | q2 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q2 == No ) ]
                            ]
                            [ text "No" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/en/1" ] [ text "Back" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "2/6" ]
                        , case answers.q2 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "Next" ]

                            _ ->
                                a [ class "button is-link", href "/en/3" ] [ text "Next" ]
                        ]
                    ]
                ]
            ]
        ]


viewEnQ3 : Answers -> Html Msg
viewEnQ3 answers =
    div []
        [ viewStep 3
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Are you experiencing any of the following:"
                    , ul []
                        [ li [] [ text "Fever" ]
                        , li [] [ text "Cough" ]
                        , li [] [ text "Sneezing" ]
                        , li [] [ text "Sore throat" ]
                        , li [] [ text "Difficulty breathing" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "buttons is-centered has-addons are-medium" ]
                        [ button
                            [ onClick (SetAnswers { answers | q3 = Yes })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q3 == Yes ) ]
                            ]
                            [ text "Yes" ]
                        , button
                            [ onClick (SetAnswers { answers | q3 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q3 == No ) ]
                            ]
                            [ text "No" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/en/2" ] [ text "Back" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "3/6" ]
                        , case answers.q3 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "Next" ]

                            _ ->
                                a [ class "button is-link", href "/en/4" ] [ text "Next" ]
                        ]
                    ]
                ]
            ]
        ]


viewEnQ4 : Answers -> Html Msg
viewEnQ4 answers =
    div []
        [ viewStep 4
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Have you travelled to any countries outside Canada (including the United States) within the last 14 days?"
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "buttons is-centered has-addons are-medium" ]
                        [ button
                            [ onClick (SetAnswers { answers | q4 = Yes })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q4 == Yes ) ]
                            ]
                            [ text "Yes" ]
                        , button
                            [ onClick (SetAnswers { answers | q4 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q4 == No ) ]
                            ]
                            [ text "No" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/en/3" ] [ text "Back" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "4/6" ]
                        , case answers.q4 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "Next" ]

                            _ ->
                                a [ class "button is-link", href "/en/5" ] [ text "Next" ]
                        ]
                    ]
                ]
            ]
        ]


viewEnQ5 : Answers -> Html Msg
viewEnQ5 answers =
    div []
        [ viewStep 5
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Did you "
                    , strong [] [ text "provide care " ]
                    , text "or have "
                    , strong [] [ text "close contact " ]
                    , text "with a person with COVID-19 (probable or confirmed) while they were ill (cough, fever, sneezing, or sore throat)?"
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "buttons is-centered has-addons are-medium" ]
                        [ button
                            [ onClick (SetAnswers { answers | q5 = Yes })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q5 == Yes ) ]
                            ]
                            [ text "Yes" ]
                        , button
                            [ onClick (SetAnswers { answers | q5 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q5 == No ) ]
                            ]
                            [ text "No" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/en/4" ] [ text "Back" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "5/6" ]
                        , case answers.q5 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "Next" ]

                            _ ->
                                a [ class "button is-link", href "/en/6" ] [ text "Next" ]
                        ]
                    ]
                ]
            ]
        ]


viewEnQ6 : Answers -> Html Msg
viewEnQ6 answers =
    div []
        [ viewStep 6
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Did you have "
                    , strong [] [ text "close contact " ]
                    , text "with a person who travelled outside of Canada in the last 14 days who has become ill (cough, fever, sneezing, or sore throat)?"
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "buttons is-centered has-addons are-medium" ]
                        [ button
                            [ onClick (SetAnswers { answers | q6 = Yes })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q6 == Yes ) ]
                            ]
                            [ text "Yes" ]
                        , button
                            [ onClick (SetAnswers { answers | q6 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q6 == No ) ]
                            ]
                            [ text "No" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/en/5" ] [ text "Back" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "6/6" ]
                        , case answers.q6 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "Next" ]

                            _ ->
                                a [ class "button is-link", href "/en/results" ] [ text "Next" ]
                        ]
                    ]
                ]
            ]
        ]


viewEnResults : Answers -> Html Msg
viewEnResults answers =
    let
        answer456 =
            div [ class "section" ]
                [ h3 [ class "title" ]
                    [ text "Please "
                    , strong [] [ text "self-isolate" ]
                    , text " and call 811 to speak with Info-Santé"
                    ]
                , div [ class "content" ]
                    [ text "A nurse at Info-Santé will speak to you about your symptoms and recent exposures to determine whether you need to be tested for COVID-19. 811 is currently experiencing heavy call volumes and we'll get to your call as quickly as we can."
                    , div [ class "mt-1" ] [ strong [] [ text "Please do not go to an emergency department, family doctor or walk-in clinic unless your symptoms worsen." ] ]
                    , text "Because you have (or had) symptoms, you should "
                    , a [ target "__blank", href "https://www.publichealthontario.ca/-/media/documents/ncov/factsheet-covid-19-how-to-self-isolate.pdf?la=en" ] [ text "self-isolate" ]
                    , text " or 14 days. That means not going to any public places, staying at home, and not having any visitors. Don’t share personal items like dishes, utensils, or towels, and wash your hands often."
                    ]
                ]
    in
    case answers.q1 of
        NotAnswered ->
            div [ class "section" ]
                [ text "Question 1 not answered. "
                , a [ href "/en/1" ] [ text "Go answer it" ]
                ]

        Yes ->
            div [ class "section" ]
                [ h3 [ class "title" ] [ text "Please call 911 or go directly to your nearest emergency department." ]
                , div [ class "content" ]
                    [ text "These symptoms require immediate attention. You should call 911 immediately, or go directly to your nearest emergency department."
                    ]
                ]

        No ->
            case answers.q2 of
                NotAnswered ->
                    div [ class "section" ]
                        [ text "Question 2 not answered. "
                        , a [ href "/en/2" ] [ text "Go answer it" ]
                        ]

                Yes ->
                    div [ class "section" ]
                        [ h3 [ class "title" ] [ text "Please call 811 to speak with Info-Santé" ]
                        , div [ class "content" ]
                            [ text "A nurse at Info-Santé will need to speak to you about your symptoms in more detail. We are experiencing heavy call volumes and will get to your call as quickly as we can."
                            ]
                        ]

                No ->
                    case answers.q3 of
                        NotAnswered ->
                            div [ class "section" ]
                                [ text "Question 3 not answered. "
                                , a [ href "/en/3" ] [ text "Go answer it" ]
                                ]

                        Yes ->
                            div [ class "section" ]
                                [ div [ class "content" ]
                                    [ strong [] [ text "Please stay at home. " ]
                                    , text "As a precaution, the Ministry of Health is asking "
                                    , strong [] [ text "anyone with symptoms " ]
                                    , text "(fever, cough, sneezing, sore throat, or difficulty breathing) to "
                                    , strong [] [ text "stay home for 14 days." ]
                                    ]
                                ]

                        No ->
                            case answers.q4 of
                                NotAnswered ->
                                    div [ class "section" ]
                                        [ text "Question 4 not answered. "
                                        , a [ href "/en/4" ] [ text "Go answer it" ]
                                        ]

                                Yes ->
                                    answer456

                                No ->
                                    case answers.q5 of
                                        NotAnswered ->
                                            div [ class "section" ]
                                                [ text "Question 5 not answered. "
                                                , a [ href "/en/5" ] [ text "Go answer it" ]
                                                ]

                                        Yes ->
                                            answer456

                                        No ->
                                            case answers.q6 of
                                                NotAnswered ->
                                                    div [ class "section" ]
                                                        [ text "Question 6 not answered. "
                                                        , a [ href "/en/6" ] [ text "Go answer it" ]
                                                        ]

                                                Yes ->
                                                    answer456

                                                No ->
                                                    div [ class "section" ]
                                                        [ h3 [ class "title" ] [ text "Please stay home. You do not need testing for COVID-19" ]
                                                        , div []
                                                            [ text "There are many common viruses other than COVID-19 that can cause your symptoms. Based on your responses you do not need to be tested for COVID-19 at this time. However, the Ministry of Health is asking"
                                                            , strong [] [ text " anyone with symptoms " ]
                                                            , text "(fever, cough, sneezing, sore throat, or difficulty breathing) to "
                                                            , strong [] [ text "stay home from work and/or school for 14 days" ]
                                                            , text ", and avoid going out in public where possible."
                                                            ]
                                                        , div [ class "mt-1" ] [ text "You can return to this self-assessment at any time if you become aware of possible exposures to COVID-19, or if your symptoms change." ]
                                                        ]


viewFrQ1 : Html Msg
viewFrQ1 =
    text "fr q1"


viewFrQ2 : Html Msg
viewFrQ2 =
    text "fr q2"


viewFrQ3 : Html Msg
viewFrQ3 =
    text "fr q3"


viewFrQ4 : Html Msg
viewFrQ4 =
    text "fr q4"


viewFrQ5 : Html Msg
viewFrQ5 =
    text "fr q5"


viewFrQ6 : Html Msg
viewFrQ6 =
    text "fr q6"


view : Model -> Browser.Document Msg
view model =
    { title = "Quebec | Self Assessment"
    , body =
        [ viewNav model.lang
        , main_ []
            [ case model.page of
                Home ->
                    viewHome

                EnQuestion num ->
                    case num of
                        1 ->
                            viewEnQ1 model.answers

                        2 ->
                            viewEnQ2 model.answers

                        3 ->
                            viewEnQ3 model.answers

                        4 ->
                            viewEnQ4 model.answers

                        5 ->
                            viewEnQ5 model.answers

                        6 ->
                            viewEnQ6 model.answers

                        _ ->
                            text "Page not found"

                FrQuestion num ->
                    case num of
                        1 ->
                            viewFrQ1

                        2 ->
                            viewFrQ2

                        3 ->
                            viewFrQ3

                        4 ->
                            viewFrQ4

                        5 ->
                            viewFrQ5

                        6 ->
                            viewFrQ6

                        _ ->
                            text "Page non trouvée"

                EnResults ->
                    viewEnResults model.answers

                FrResults ->
                    text "fr results"
            ]
        ]
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
