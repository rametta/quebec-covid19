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


viewNav : Page -> Lang -> Html Msg
viewNav page lang =
    nav [ class "navbar qc-nav has-text-white is-flex", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item has-text-white", href "/" ]
                [ text "Quebec | Self Assessment"
                ]
            ]
        , let
            results =
                div [ class "buttons has-addons mr-1" ]
                    [ a [ class "button is-small", classList [ ( "is-link is-selected", lang == En ) ], href <| "/en/results", attribute "aria-label" "English" ] [ text "EN" ]
                    , a [ class "button is-small", classList [ ( "is-link is-selected", lang == Fr ) ], href <| "/fr/resultats", attribute "aria-label" "Francais" ] [ text "FR" ]
                    ]

            question i =
                div [ class "buttons has-addons mr-1" ]
                    [ a [ class "button is-small", classList [ ( "is-link is-selected", lang == En ) ], href <| "/en/" ++ String.fromInt i, attribute "aria-label" "English" ] [ text "EN" ]
                    , a [ class "button is-small", classList [ ( "is-link is-selected", lang == Fr ) ], href <| "/fr/" ++ String.fromInt i, attribute "aria-label" "Francais" ] [ text "FR" ]
                    ]
          in
          case page of
            Home ->
                div [ class "buttons has-addons mr-1" ]
                    [ a [ class "button is-small", classList [ ( "is-link is-selected", lang == En ) ], href "/en/1", attribute "aria-label" "English" ] [ text "EN" ]
                    , a [ class "button is-small", classList [ ( "is-link is-selected", lang == Fr ) ], href "/fr/1", attribute "aria-label" "Francais" ] [ text "FR" ]
                    ]

            EnQuestion i ->
                question i

            FrQuestion i ->
                question i

            EnResults ->
                results

            FrResults ->
                results
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
        noTesting =
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

        selfIsolate =
            div [ class "section" ]
                [ h3 [ class "title" ]
                    [ text "Please"
                    , strong [] [ text " self-isolate " ]
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

        selfIsolateNoSymptoms =
            div [ class "section" ]
                [ h3 [ class "title" ]
                    [ text "Please"
                    , strong [] [ text " self-isolate. " ]
                    , text "You do not need testing for COVID-19."
                    ]
                , div [ class "content" ]
                    [ text "Since you don't have symptoms, you do not need testing for COVID-19 at this time. However, there’s a chance you could get sick since it’s less than 14 days since your exposure. You should self-monitor for any symptoms (fever, cough, sneezing, sore throat, or difficulty breathing). If you begin to develop these, you should take this self-assessment again."
                    , div [] [ strong [] [ text "As an additional precautionary measure," ] ]
                    , text "the Ministry of Health is asking those returning from travel outside Canada to "
                    , a [ target "__blank", href "https://www.publichealthontario.ca/-/media/documents/ncov/factsheet-covid-19-how-to-self-isolate.pdf?la=en" ] [ text "self-isolate" ]
                    , text " or 14 days. That means not going to any public places, staying at home, and not having any visitors. Don’t share personal items like dishes, utensils, or towels, and wash your hands often."
                    ]
                ]

        selfMonitor =
            div [ class "section" ]
                [ h3 [ class "title" ]
                    [ text "Since you don’t have any COVID-19 symptoms, you don’t need to be tested for COVID-19."
                    ]
                , div [ class "content" ]
                    [ text "However, there’s a chance you could get sick since it’s less than 14 days since your exposure. You should self-monitor for any symptoms (fever, cough, sneezing, sore throat, or difficulty breathing)."
                    , div []
                        [ text "If you begin to develop symptoms (fever, cough, sneezing, sore throat, or difficulty breathing), you should "
                        , a [ target "__blank", href "https://www.publichealthontario.ca/-/media/documents/ncov/factsheet-covid-19-how-to-self-isolate.pdf?la=en" ] [ text "self-isolate" ]
                        , text " and take this self-assessment again. "
                        ]
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
                            case answers.q4 of
                                NotAnswered ->
                                    div [ class "section" ]
                                        [ text "Question 4 not answered. "
                                        , a [ href "/en/4" ] [ text "Go answer it" ]
                                        ]

                                Yes ->
                                    selfIsolate

                                No ->
                                    case answers.q5 of
                                        NotAnswered ->
                                            div [ class "section" ]
                                                [ text "Question 5 not answered. "
                                                , a [ href "/en/5" ] [ text "Go answer it" ]
                                                ]

                                        Yes ->
                                            selfIsolate

                                        No ->
                                            case answers.q6 of
                                                NotAnswered ->
                                                    div [ class "section" ]
                                                        [ text "Question 6 not answered. "
                                                        , a [ href "/en/6" ] [ text "Go answer it" ]
                                                        ]

                                                Yes ->
                                                    selfIsolate

                                                No ->
                                                    noTesting

                        No ->
                            case answers.q4 of
                                NotAnswered ->
                                    div [ class "section" ]
                                        [ text "Question 4 not answered. "
                                        , a [ href "/en/4" ] [ text "Go answer it" ]
                                        ]

                                Yes ->
                                    selfIsolateNoSymptoms

                                No ->
                                    case answers.q5 of
                                        NotAnswered ->
                                            div [ class "section" ]
                                                [ text "Question 5 not answered. "
                                                , a [ href "/en/5" ] [ text "Go answer it" ]
                                                ]

                                        Yes ->
                                            selfMonitor

                                        No ->
                                            case answers.q6 of
                                                NotAnswered ->
                                                    div [ class "section" ]
                                                        [ text "Question 6 not answered. "
                                                        , a [ href "/en/6" ] [ text "Go answer it" ]
                                                        ]

                                                Yes ->
                                                    selfMonitor

                                                No ->
                                                    selfMonitor


viewFrQ1 : Answers -> Html Msg
viewFrQ1 answers =
    div []
        [ viewStep 1
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Êtes-vous confronté à l'un des problèmes suivants:"
                    , ul []
                        [ li [] [ text "Difficulté à respirer sévère (par exemple, difficulté à respirer ou à parler avec des mots simples)" ]
                        , li [] [ text "Douleur thoracique sévère" ]
                        , li [] [ text "Avoir du mal à se réveiller" ]
                        , li [] [ text "Se sentir confus" ]
                        , li [] [ text "Perdre conscience" ]
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
                            [ text "oui" ]
                        , button
                            [ onClick (SetAnswers { answers | q1 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q1 == No ) ]
                            ]
                            [ text "non" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/" ] [ text "accueil" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "1/6" ]
                        , case answers.q1 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "suivant" ]

                            _ ->
                                a [ class "button is-link", href "/fr/2" ] [ text "suivant" ]
                        ]
                    ]
                ]
            ]
        ]


viewFrQ2 : Answers -> Html Msg
viewFrQ2 answers =
    div []
        [ viewStep 2
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Êtes-vous confronté à l'un des problèmes suivants:"
                    , ul []
                        [ li [] [ text "Essoufflement au repos" ]
                        , li [] [ text "Incapacité à se coucher en raison de difficultés respiratoires" ]
                        , li [] [ text "Problèmes de santé chroniques que vous avez du mal à gérer en raison de difficultés respiratoires" ]
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
                            [ text "oui" ]
                        , button
                            [ onClick (SetAnswers { answers | q2 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q2 == No ) ]
                            ]
                            [ text "non" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/fr/1" ] [ text "retour" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "2/6" ]
                        , case answers.q2 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "suivant" ]

                            _ ->
                                a [ class "button is-link", href "/fr/3" ] [ text "suivant" ]
                        ]
                    ]
                ]
            ]
        ]


viewFrQ3 : Answers -> Html Msg
viewFrQ3 answers =
    div []
        [ viewStep 3
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Êtes-vous confronté à l'un des problèmes suivants:"
                    , ul []
                        [ li [] [ text "Fièvre" ]
                        , li [] [ text "La toux" ]
                        , li [] [ text "Éternuements" ]
                        , li [] [ text "Gorge irritée" ]
                        , li [] [ text "Difficulté à respirer" ]
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
                            [ text "oui" ]
                        , button
                            [ onClick (SetAnswers { answers | q3 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q3 == No ) ]
                            ]
                            [ text "non" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/fr/2" ] [ text "retour" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "3/6" ]
                        , case answers.q3 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "suivant" ]

                            _ ->
                                a [ class "button is-link", href "/fr/4" ] [ text "suivant" ]
                        ]
                    ]
                ]
            ]
        ]


viewFrQ4 : Answers -> Html Msg
viewFrQ4 answers =
    div []
        [ viewStep 4
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Avez-vous voyagé dans des pays à l'extérieur du Canada (y compris les États-Unis) au cours des 14 derniers jours?"
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "buttons is-centered has-addons are-medium" ]
                        [ button
                            [ onClick (SetAnswers { answers | q4 = Yes })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q4 == Yes ) ]
                            ]
                            [ text "oui" ]
                        , button
                            [ onClick (SetAnswers { answers | q4 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q4 == No ) ]
                            ]
                            [ text "non" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/fr/3" ] [ text "retour" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "4/6" ]
                        , case answers.q4 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "suivant" ]

                            _ ->
                                a [ class "button is-link", href "/fr/5" ] [ text "suivant" ]
                        ]
                    ]
                ]
            ]
        ]


viewFrQ5 : Answers -> Html Msg
viewFrQ5 answers =
    div []
        [ viewStep 5
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Avez-vous "
                    , strong [] [ text "prodigué des soins " ]
                    , text "ou été "
                    , strong [] [ text "en contact étroit " ]
                    , text "avec une personne atteinte de COVID-19 (probable ou confirmée) alors qu'elle était malade (toux, fièvre, éternuements ou mal de gorge)?"
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "buttons is-centered has-addons are-medium" ]
                        [ button
                            [ onClick (SetAnswers { answers | q5 = Yes })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q5 == Yes ) ]
                            ]
                            [ text "oui" ]
                        , button
                            [ onClick (SetAnswers { answers | q5 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q5 == No ) ]
                            ]
                            [ text "non" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/fr/4" ] [ text "retour" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "5/6" ]
                        , case answers.q5 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "suivant" ]

                            _ ->
                                a [ class "button is-link", href "/fr/6" ] [ text "suivant" ]
                        ]
                    ]
                ]
            ]
        ]


viewFrQ6 : Answers -> Html Msg
viewFrQ6 answers =
    div []
        [ viewStep 6
        , div [ class "section" ]
            [ div [ class "columns is-centered is-multiline" ]
                [ div [ class "column is-8 content" ]
                    [ text "Avez-vous eu des "
                    , strong [] [ text "contacts étroits " ]
                    , text "avec une personne qui a voyagé à l'extérieur du Canada au cours des 14 derniers jours et qui est tombée malade (toux, fièvre, éternuements ou mal de gorge)?"
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "buttons is-centered has-addons are-medium" ]
                        [ button
                            [ onClick (SetAnswers { answers | q6 = Yes })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q6 == Yes ) ]
                            ]
                            [ text "oui" ]
                        , button
                            [ onClick (SetAnswers { answers | q6 = No })
                            , type_ "button"
                            , class "button"
                            , classList [ ( "is-link is-selected", answers.q6 == No ) ]
                            ]
                            [ text "non" ]
                        ]
                    ]
                , div [ class "column is-8" ]
                    [ div [ class "is-flex step-btns mt-1" ]
                        [ a [ class "button is-text", href "/fr/5" ] [ text "retour" ]
                        , span [ class "is-family-monospace is-size-7" ] [ text "6/6" ]
                        , case answers.q6 of
                            NotAnswered ->
                                button [ class "button", type_ "button", disabled True ] [ text "suivant" ]

                            _ ->
                                a [ class "button is-link", href "/fr/resultats" ] [ text "suivant" ]
                        ]
                    ]
                ]
            ]
        ]


viewFrResults : Answers -> Html Msg
viewFrResults answers =
    let
        noTesting =
            div [ class "section" ]
                [ h3 [ class "title" ] [ text "Veuillez rester à la maison. Vous n'avez pas besoin de tester COVID-19" ]
                , div []
                    [ text "Il existe de nombreux virus courants autres que COVID-19 qui peuvent provoquer vos symptômes. Sur la base de vos réponses, vous n'avez pas besoin d'être testé pour COVID-19 pour le moment. Cependant, le ministère de la Santé de la Colombie-Britannique demande à toute personne présentant des symptômes (fièvre, toux, éternuements, mal de gorge ou difficulté à respirer) de rester à la maison du travail et / ou de l'école pendant 14 jours et d'éviter de sortir en public si possible."
                    ]
                , div [ class "mt-1" ] [ text "Vous pouvez revenir à cette auto-évaluation à tout moment si vous vous rendez compte d'expositions possibles à COVID-19 ou si vos symptômes changent." ]
                ]

        selfIsolate =
            div [ class "section" ]
                [ h3 [ class "title" ]
                    [ text "Veuillez vous isoler et composez le 811 pour parler à Info-Santé"
                    ]
                , div [ class "content" ]
                    [ text "Une infirmière d'Info-Santé vous parlera de vos symptômes et de vos expositions récentes pour déterminer si vous devez subir un test de COVID-19. Le 811 connaît actuellement de gros volumes d'appels et nous répondrons à votre appel le plus rapidement possible."
                    , div [ class "mt-1" ] [ strong [] [ text "Veuillez ne pas vous rendre à l'urgence, au médecin de famille ou à la clinique sans rendez-vous à moins que vos symptômes ne s'aggravent." ] ]
                    , text "Parce que vous avez (ou avez eu) des symptômes, vous devez vous "
                    , a [ target "__blank", href "https://www.publichealthontario.ca/-/media/documents/ncov/factsheet-covid-19-how-to-self-isolate.pdf?la=fr" ] [ text "auto-isole" ]
                    , text " pendant 14 jours. Cela signifie ne pas aller dans les lieux publics, rester à la maison et ne pas avoir de visiteurs. Ne partagez pas d'objets personnels comme la vaisselle, les ustensiles ou les serviettes et lavez-vous les mains souvent."
                    ]
                ]

        selfIsolateNoSymptoms =
            div [ class "section" ]
                [ h3 [ class "title" ]
                    [ text "Veuillez vous"
                    , strong [] [ text " isoler. " ]
                    , text "Veuillez vous isoler. Vous n'avez pas besoin de tester COVID-19."
                    ]
                , div [ class "content" ]
                    [ text "Comme vous ne présentez aucun symptôme, vous n'avez pas besoin de tester le COVID-19 pour le moment. Cependant, il est possible que vous tombiez malade, car il y a moins de 14 jours depuis votre exposition. Vous devez surveiller vous-même les symptômes (fièvre, toux, éternuements, mal de gorge ou difficulté à respirer). Si vous commencez à les développer, vous devriez recommencer cette auto-évaluation."
                    , div [] [ strong [] [ text "Par mesure de précaution supplémentaire," ] ]
                    , text "the Ministry of Health is asking those returning from travel outside Canada to "
                    , a [ target "__blank", href "https://www.publichealthontario.ca/-/media/documents/ncov/factsheet-covid-19-how-to-self-isolate.pdf?la=fr" ] [ text "self-isolate" ]
                    , text " or 14 days. That means not going to any public places, staying at home, and not having any visitors. Don’t share personal items like dishes, utensils, or towels, and wash your hands often."
                    ]
                ]

        selfMonitor =
            div [ class "section" ]
                [ h3 [ class "title" ]
                    [ text "Since you don’t have any COVID-19 symptoms, you don’t need to be tested for COVID-19."
                    ]
                , div [ class "content" ]
                    [ text "However, there’s a chance you could get sick since it’s less than 14 days since your exposure. You should self-monitor for any symptoms (fever, cough, sneezing, sore throat, or difficulty breathing)."
                    , div []
                        [ text "If you begin to develop symptoms (fever, cough, sneezing, sore throat, or difficulty breathing), you should "
                        , a [ target "__blank", href "https://www.publichealthontario.ca/-/media/documents/ncov/factsheet-covid-19-how-to-self-isolate.pdf?la=en" ] [ text "self-isolate" ]
                        , text " and take this self-assessment again. "
                        ]
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
                            case answers.q4 of
                                NotAnswered ->
                                    div [ class "section" ]
                                        [ text "Question 4 not answered. "
                                        , a [ href "/en/4" ] [ text "Go answer it" ]
                                        ]

                                Yes ->
                                    selfIsolate

                                No ->
                                    case answers.q5 of
                                        NotAnswered ->
                                            div [ class "section" ]
                                                [ text "Question 5 not answered. "
                                                , a [ href "/en/5" ] [ text "Go answer it" ]
                                                ]

                                        Yes ->
                                            selfIsolate

                                        No ->
                                            case answers.q6 of
                                                NotAnswered ->
                                                    div [ class "section" ]
                                                        [ text "Question 6 not answered. "
                                                        , a [ href "/en/6" ] [ text "Go answer it" ]
                                                        ]

                                                Yes ->
                                                    selfIsolate

                                                No ->
                                                    noTesting

                        No ->
                            case answers.q4 of
                                NotAnswered ->
                                    div [ class "section" ]
                                        [ text "Question 4 not answered. "
                                        , a [ href "/en/4" ] [ text "Go answer it" ]
                                        ]

                                Yes ->
                                    selfIsolateNoSymptoms

                                No ->
                                    case answers.q5 of
                                        NotAnswered ->
                                            div [ class "section" ]
                                                [ text "Question 5 not answered. "
                                                , a [ href "/en/5" ] [ text "Go answer it" ]
                                                ]

                                        Yes ->
                                            selfMonitor

                                        No ->
                                            case answers.q6 of
                                                NotAnswered ->
                                                    div [ class "section" ]
                                                        [ text "Question 6 not answered. "
                                                        , a [ href "/en/6" ] [ text "Go answer it" ]
                                                        ]

                                                Yes ->
                                                    selfMonitor

                                                No ->
                                                    selfMonitor


view : Model -> Browser.Document Msg
view model =
    { title = "Quebec | Self Assessment"
    , body =
        [ viewNav model.page model.lang
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
                            viewFrQ1 model.answers

                        2 ->
                            viewFrQ2 model.answers

                        3 ->
                            viewFrQ3 model.answers

                        4 ->
                            viewFrQ4 model.answers

                        5 ->
                            viewFrQ5 model.answers

                        6 ->
                            viewFrQ6 model.answers

                        _ ->
                            text "Page non trouvée"

                EnResults ->
                    viewEnResults model.answers

                FrResults ->
                    viewFrResults model.answers
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
