module Form.Builder exposing
    ( Field
    , cancel
    , checkbox
    , conditional
    , conditionalBool
    , conditionalInList
    , date
    , email
    , field
    , form
    , formRow
    , group
    , html
    , list
    , optgroup
    , password
    , pnr
    , save
    , select
    , selectString
    , selectWithoutBlank
    , set
    , showIf
    , submit
    , switch
    , tel
    , time
    , txt
    , txt_
    , txtarea
    , wrapper
    )

import Form exposing (FieldState, Form, InputType(..), Msg(..))
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode


type alias State e a =
    { form : Form e a
    , errorString : ErrorString e
    , path : List String
    }


type alias Field e a =
    State e a -> Html Msg


type alias ErrorString e =
    ErrorValue e -> String


{-| A live validating form.

    form state
        FormMsg
        [ email "Användarnamn" "username"
        , password "Lösenord" "password"
        , submit "Logga in" isLoading
        ]

-}
form : Form e a -> ErrorString e -> (Msg -> b) -> List (Field e a) -> Html b
form formState errorString msg fields =
    let
        state =
            State formState errorString []
    in
    Html.map msg <|
        Html.form
            [ onSubmit Form.Submit ]
            (List.map (\f -> f state) fields)


{-| Generic text input.
-}
txt : String -> String -> State e a -> Html Msg
txt title name state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.textInput f
            [ class "form-control"
            , id f.path
            ]


{-| Generic text input (without a label).
-}
txt_ : String -> String -> State e a -> Html Msg
txt_ title name state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    fieldset
        [ classList
            [ ( "form-group", True )
            , ( "is-invalid", f.liveError /= Nothing )
            ]
        ]
        [ Input.textInput f
            [ class "form-control"
            , id f.path
            , placeholder title
            ]
        , error state.errorString f
        ]


{-| Generic textarea.
-}
txtarea : String -> String -> State e a -> Html Msg
txtarea title name state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.textArea f
            [ class "form-control"
            , id f.path
            ]


{-| A text box for inputing an email address.
-}
email : String -> String -> State e a -> Html Msg
email title name state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.baseInput "email"
            String
            Text
            f
            [ class "form-control"
            , id f.path
            ]


{-| A text box for inputing a phone number.
-}
tel : String -> String -> State e a -> Html Msg
tel title name state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.baseInput "tel"
            String
            Text
            f
            [ class "form-control"
            , id f.path
            ]


{-| A password input.
-}
password : String -> String -> State e a -> Html Msg
password title name state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.passwordInput f
            [ class "form-control"
            , id f.path
            ]


{-| A text box for inputting a social security number.
-}
pnr : String -> String -> State e a -> Html Msg
pnr title name state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.baseInput "tel"
            String
            Text
            f
            [ placeholder "ååååmmddxxxx"
            , autocomplete False
            , class "form-control"
            , id f.path
            ]


{-| Private
-}
radio : Form.FieldState e String -> ( String, String ) -> Html Msg
radio fieldState ( value, title ) =
    label [ class "form-check-label" ]
        [ Input.radioInput value
            fieldState
            [ class "form-check-input"
            , id fieldState.path
            ]
        , text title
        ]


{-| A switch widget containing multiple radio buttons.

    switch "Välj kontotyp"
        "account_type"
        [ ( "free", "Gratis" )
        , ( "standard", "Standard" )
        , ( "pro", "Professionell" )
        ]

-}
switch : String -> String -> List ( String, String ) -> State e a -> Html Msg
switch title name values state =
    let
        f =
            Form.getFieldAsString (path name state) state.form

        input v =
            div
                [ classList
                    [ ( "form-check", True )
                    , ( "form-check-inline", List.length values <= 2 )
                    ]
                ]
                [ radio f v ]
    in
    field f title state.errorString <|
        div [ class "switch", id f.path, tabindex -1 ] <|
            List.map input values


{-| A checkbox.
-}
checkbox : String -> String -> State e a -> Html Msg
checkbox title name state =
    let
        f =
            Form.getFieldAsBool (path name state) state.form
    in
    fieldset
        [ classList
            [ ( "form-group", True )
            , ( "form-check", True )
            , ( "is-invalid", f.liveError /= Nothing )
            ]
        ]
        [ label [ class "form-check-label" ]
            [ Input.checkboxInput f [ class "form-check-input", id f.path ]
            , text title
            ]
        , error state.errorString f
        ]


{-| A date picker
-}
date : String -> String -> State e a -> Html Msg
date title name state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.baseInput "text"
            String
            Text
            f
            [ placeholder "åååå-mm-dd"
            , onChange (String >> Input f.path Text)
            , class "date-picker form-control"
            , id f.path
            ]


{-| A time picker.
-}
time : String -> String -> State e a -> Html Msg
time title name state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.baseInput "text"
            String
            Text
            f
            [ placeholder "hh:mm"
            , class "time-picker form-control"
            , id f.path
            ]


{-| A select box.

    select "Välj en färg"
        "color"
        [ ( "red", "Röd" )
        , ( "green", "Grön" )
        , ( "blue", "Blå" )
        ]

-}
select : String -> String -> List ( String, String ) -> State e a -> Html Msg
select title name values state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.selectInput ([ ( "", "" ) ] ++ values)
            f
            [ class "form-control custom-select"
            , id f.path
            ]


{-| A select box without a "blank" alternative

    selectWithoutBlank "Välj en färg"
        "color"
        [ ( "red", "Röd" )
        , ( "green", "Grön" )
        , ( "blue", "Blå" )
        ]

-}
selectWithoutBlank : String -> String -> List ( String, String ) -> State e a -> Html Msg
selectWithoutBlank title name values state =
    let
        f =
            Form.getFieldAsString (path name state) state.form
    in
    field f title state.errorString <|
        Input.selectInput values
            f
            [ class "form-control custom-select"
            , id f.path
            ]


{-| A select box where the alternatives are taken as values as well.

    select "Välj en färg"
        "color"
        [ "red", "green", "blue" ]

-}
selectString : String -> String -> List String -> State e a -> Html Msg
selectString title name values =
    select title name (List.map2 (\a b -> ( a, b )) values values)


{-| A select box with grouped options.

    select "Välj en stad" "city"
      [ ( "Sverige",
        [ ( "stockholm", "Stockholm" )
        , ( "goteborg", "Göteborg" )
        ]
      , ( "Danmark",
        [ ( "kopenhamn", "Köpenhamn" ) ]
        )
      ]

-}
optgroup : String -> String -> List ( String, List ( String, String ) ) -> State e a -> Html Msg
optgroup title name values state =
    let
        f =
            Form.getFieldAsString (path name state) state.form

        attr =
            [ on "change"
                (targetValue |> Json.Decode.map (String >> Input f.path Select))
            , onFocus (Focus f.path)
            , onBlur (Blur f.path)
            , class "form-control"
            , id f.path
            ]

        buildOptGroup : ( String, List ( String, String ) ) -> Html Msg
        buildOptGroup ( label, options ) =
            Html.optgroup [ attribute "label" label ] (List.map buildOption options)

        buildOption ( k, v ) =
            Html.option [ value k, selected (f.value == Just k) ] [ text v ]

        selectInput options =
            Html.select attr ([ buildOption ( "", "" ) ] ++ List.map buildOptGroup options)
    in
    field f title state.errorString (selectInput values)


{-| A submit button for the form.
-}
submit : String -> State e a -> Html Msg
submit title state =
    input
        [ type_ "submit"
        , class "btn btn-primary btn-lg btn-block"
        , value title
        ]
        []


{-| A save button for the form.
-}
save : String -> State e a -> Html Msg
save title state =
    button [ class "btn btn-primary" ] [ text title ]


{-| A cancel (reset) button for the form.
-}
cancel : String -> State e a -> Html Msg
cancel title state =
    input [ type_ "reset", class "btn btn-secondary", value title ] []


{-| A group of fields.

    group "person"
        [ txt "Namn" "name"
        , tel "Telefonnummer" "phone"
        ]

-}
group : String -> List (Field e a) -> State e a -> Html Msg
group name fields state =
    let
        state2 =
            { state | path = state.path ++ [ name ] }
    in
    div [ id (path name state), tabindex -1 ] (List.map (\f -> f state2) fields)


{-| A list of sub forms with an append button to add more items.

    list ( "Lägg till kontakt", "Ta bort" )
        "contacts"
        [ txt "Namn" "name"
        , tel "Telefonnummer" "phone"
        , email "E-post" "email"
        ]

-}
list : String -> ( String, String, String ) -> String -> List (Field e a) -> State e a -> Html Msg
list title ( addItem, addMoreItems, removeItem ) name fields state =
    let
        name2 =
            path name state

        f =
            Form.getFieldAsString name2 state.form

        indexes =
            Form.getListIndexes name2 state.form

        state2 =
            { state | path = state.path ++ [ name ] }

        addText =
            if List.isEmpty indexes then
                addItem

            else
                addMoreItems

        item i =
            li
                [ class "list-group-item" ]
                [ group (String.fromInt i) fields state2
                , a
                    [ onClick (Form.RemoveItem name2 i)
                    , class "ion-md-trash btn btn-light"
                    ]
                    [ text removeItem ]
                ]
    in
    field f title state.errorString <|
        div [ class "mt-2", id (path name state), tabindex -1 ]
            [ if List.isEmpty indexes then
                text ""

              else
                ul [ class "list-group" ] (List.map item indexes)
            , a
                [ onClick (Form.Append name2)
                , classList
                    [ ( "ion-md-add btn btn-secondary text-light", True )
                    , ( "mt-3", List.isEmpty indexes == False )
                    ]
                ]
                [ text addText ]
            ]


{-| A list of unique values that can each be toggled on/off.
-}
set : String -> String -> List ( String, String, Maybe String ) -> State e a -> Html Msg
set title name values state =
    let
        f =
            Form.getFieldAsString (path name state) state.form

        state2 =
            { state | path = state.path ++ [ name ] }

        item ( value, title_, description ) =
            let
                f2 =
                    Form.getFieldAsBool (path value state2) state2.form
            in
            div
                [ class "form-check" ]
                [ label
                    [ class "form-check-label" ]
                    ([ Input.checkboxInput f2
                        [ class "form-check-input"
                        , id f.path
                        ]
                     , text title_
                     ]
                        ++ itemDesc description
                    )
                ]

        itemDesc description =
            case description of
                Just str ->
                    [ div
                        [ class "small text-muted mb-1" ]
                        [ text str ]
                    ]

                Nothing ->
                    []
    in
    field f title state.errorString <|
        div [ class "checkboxes mt-1", id (path name state), tabindex -1 ]
            (List.map item values)


{-| Conditionally display fields based on the string value of another field.

    conditional "accident_scene"
        [ ( "school", txt "Vilken skola?" "accident_scene_details" )
        , ( "work", txt "Vilken arbetsgivare?" "accident_scene_details" )
        , ( "sports", txt "Vilken idrott?" "accident_scene_details" )
        ]

-}
conditional : String -> List ( String, Field e a ) -> State e a -> Html Msg
conditional target fields state =
    let
        f =
            Form.getFieldAsString (path target state) state.form

        cond ( value, func ) =
            if f.value == Just value then
                Just ( value, func state )

            else
                Nothing
    in
    Keyed.node "div" [] <| List.filterMap cond fields


{-| conditionalBool display fields based on the boolean value of another field.
conditionalBool "consequences.scar"
[ ( True
, txt "How big is the scar?" "scar_size" )
)
]
-}
conditionalBool : String -> List ( Bool, Field e a ) -> State e a -> Html Msg
conditionalBool target fields state =
    let
        f =
            Form.getFieldAsBool (path target state) state.form
    in
    div []
        (List.filterMap
            (\( value, func ) ->
                if f.value == Just value then
                    Just (func state)

                else
                    Nothing
            )
            fields
        )


{-| Conditionally display fields based on the string value of a field in a list.
conditionalInList "claim.expenses.name"
[ ( "dentist_visit"
, txt "Which dentist?" "dentist_office" )
)
]
-}
conditionalInList : String -> List ( String, Field e a ) -> State e a -> Html Msg
conditionalInList target fields state =
    let
        name =
            path target state

        listPath =
            String.join "." <|
                List.take (List.length (String.split "." target) - 1) <|
                    String.split "." target

        listItemName =
            Maybe.withDefault "" <|
                List.head <|
                    List.reverse <|
                        String.split "." target

        listItems =
            Form.getListIndexes listPath state.form
    in
    div []
        (List.filterMap
            (\( value, func ) ->
                let
                    isInlist =
                        List.isEmpty <|
                            List.filter
                                (\index ->
                                    let
                                        selectField =
                                            Form.getFieldAsString (listPath ++ "." ++ String.fromInt index ++ "." ++ listItemName) state.form
                                    in
                                    if selectField.value == Just value then
                                        True

                                    else
                                        False
                                )
                                listItems
                in
                if isInlist then
                    Nothing

                else
                    Just (func state)
            )
            fields
        )


{-| Similar to `conditional`, but lets you use a `case .. of` statement
instead.

Note: This helper is more flexible and can be used to express more
complicated logic than the `conditional` helper. Yet the `conditional` helper
is generally prefered in most cases, since it allows the implementation to use
keyed nodes to show and hide the form fields which makes the underlying virtual
DOM algorithm more efficient and correct (it helps avoid ghost content in input
fields). When using this helper you should instead manually use `Html.Keyed` as
needed to avoid this problem.

    showIf "accident_scene"
        (\scene ->
            case scene of
                Just "school" ->
                    [ txt "Vilken skola?" "accident_scene_details"
                    , txt "Kontaktperson" "accident_scene_contact"
                    ]

                _ ->
                    [ txt "Annan plats" "accident_scene_details" ]
        )

-}
showIf : String -> (Maybe String -> List (Field e a)) -> State e a -> Html Msg
showIf target cond state =
    let
        f =
            Form.getFieldAsString (path target state) state.form
    in
    div [] <|
        List.map (\func -> func state) (cond f.value)


{-| A helper function for inserting HTML in a form.
-}
html : Html Msg -> State e a -> Html Msg
html h state =
    h


{-| A div wrapper with a custom CSS class

    wrapper "myClass"
        [ txt "First name" "first_name"
        , txt "Last name" "last_name"
        ]

-}
wrapper : String -> List (Field e a) -> State e a -> Html Msg
wrapper cssClass fields state =
    div [ class cssClass ] (List.map (\f -> f state) fields)


{-| A form row will layout its fields in equal sized columns.

    formRow
        [ txt "Namn" "name"
        , phone "Telefonnummer" "phone"
        ]

-}
formRow : List (Field e a) -> State e a -> Html Msg
formRow columns state =
    let
        column f =
            div [ class "col-sm" ] [ f state ]
    in
    div [ class "form-row" ] <| List.map column columns


{-| A fieldset with a label and error message.
This function is only useful if you want to implement a custom field type.
-}
field : FieldState e String -> String -> ErrorString e -> Html Msg -> Html Msg
field f labeltext errorString content =
    fieldset
        [ classList
            [ ( "form-group", True )
            , ( "is-invalid", f.liveError /= Nothing )
            , ( "has-value", f.value /= Nothing && f.value /= Just "" )
            ]
        ]
        [ label [ for f.path ]
            [ span [ class "label-text" ] [ text labeltext ]
            , content
            ]
        , error errorString f
        ]


{-| Private
-}
path : String -> State e a -> String
path name state =
    String.join "." (state.path ++ [ name ])


{-| Private
-}
error : ErrorString e -> FieldState e a -> Html msg
error errorString f =
    case f.liveError of
        Just err ->
            div [ class "invalid-feedback" ] [ text (errorString err) ]

        Nothing ->
            text ""


{-| Private
-}
onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (Json.Decode.map tagger targetValue)
