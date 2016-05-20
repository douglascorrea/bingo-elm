module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import String exposing (toUpper, repeat, trimRight)


-- MODEL

type alias Entry =
    { phrase: String
    , points: Int
    , wasSpoken: Bool
    , id: Int
    }

type alias Model =
    {
        entries: List Entry
    }

newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
    { phrase = phrase
    , points = points
    , wasSpoken = False
    , id = id
    }

initialModel : Model
initialModel =
    { entries =
        [ newEntry "Doing Agile" 200 2
        , newEntry "In the Cloud" 300 3
        , newEntry "Future Proof" 100 1
        , newEntry "Rock-Star Ninja" 400 4
        ]

    }

-- UPDATE

type Msg
    = NoOp
    | Sort
    | Delete Int
    | Mark Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Sort ->
            { model | entries = List.sortBy .points model.entries}


        Delete id ->
            let
                remainingEntries =
                    List.filter (\e -> e.id /= id) model.entries
            in
                { model | entries = remainingEntries }

        Mark id ->
            let
                updateEntry e =
                    if e.id == id then { e | wasSpoken = (not e.wasSpoken) } else e
            in
                { model | entries = List.map updateEntry model.entries }

-- VIEW

title : String -> Int -> Html Msg
title message times =
    message ++ " "
        |> toUpper
        |> repeat times
        |> trimRight
        |> text

pageHeader : Html Msg
pageHeader =
    h1 [ ] [ title "bingo!" 3 ]

pageFooter : Html Msg
pageFooter =
    footer [ ]
        [ a [ href "http://github.com" ]
            [ text "Github" ] ]

entryItem : Entry -> Html Msg
entryItem entry =
    li
        [ classList [ ("highlight", entry.wasSpoken) ], onClick (Mark entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        , button
            [ class "delete", onClick (Delete entry.id) ]
            [ ]
        ]

totalPoints : List Entry -> Int
totalPoints entries =
    entries
        |> List.filter .wasSpoken
        |> List.foldl (\e sum -> sum + e.points) 0

totalItem : Int -> Html Msg
totalItem total =
    li
        [ class "total" ]
        [ span [ class "label"] [ text "Total"]
        , span [ class "points" ] [ text (toString total) ]
        ]

entryList : List Entry -> Html Msg
entryList entries =
    let
        entryItems = (List.map entryItem entries)
        items = entryItems ++ [ totalItem (totalPoints entries) ]
    in
        ul [ ] items

view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ pageHeader
        , entryList model.entries
        , button
            [ class "sort", onClick Sort  ]
            [ text "Sort" ]
        , pageFooter
        ]

-- WIRE IT ALL TOGETHER

main : Program Never
main =
    beginnerProgram
        { model = initialModel, update = update, view = view }