module Form.Builder.Validation exposing
    ( isodate
    , isotime
    , nonEmptyList
    , nonEmptySet
    , personName
    , phone
    , set
    , strictEmail
    , termsAgreement
    , yesno
    )

import Form.Builder.Error exposing (ValidationError(..), ValidationErrorPersonName(..))
import Form.Error exposing (..)
import Form.Validate exposing (..)
import Iso8601
import Parser
import Regex
import Set exposing (Set)
import Time exposing (Posix)


yesno : Form.Validate.Validation ValidationError Bool
yesno =
    string
        |> andThen
            (\str ->
                case str of
                    "yes" ->
                        succeed True

                    "no" ->
                        succeed False

                    _ ->
                        fail (customError EmptySet)
            )


isodate : Form.Validate.Validation ValidationError String
isodate =
    let
        validate str =
            if Regex.contains isoDateFormat str then
                Iso8601.toTime str
                    |> Result.mapError (\e -> InvalidISODate (Parser.deadEndsToString e))
                    |> Result.mapError customError
                    |> Result.map Iso8601.fromTime
                    |> Result.map (String.left 10)

            else
                Err (customError (InvalidISODate "Invalid format"))
    in
    customValidation string validate


personNameFormat : Regex.Regex
personNameFormat =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[^0-9][ ][^0-9]"


personName : Form.Validate.Validation ValidationError String
personName =
    let
        validate str =
            let
                name =
                    String.trim str
            in
            if Regex.contains personNameFormat name then
                Ok name

            else if String.length name >= 3 then
                Err (customError (InvalidPersonName LastNameMissing))

            else
                Err (customError (InvalidPersonName NameTooShort))
    in
    customValidation string validate


strictEmail : Form.Validate.Validation ValidationError String
strictEmail =
    let
        format =
            Maybe.withDefault Regex.never <|
                Regex.fromStringWith
                    { caseInsensitive = True
                    , multiline = False
                    }
                    "^[A-Z0-9._%+-]+@(?:[A-Z0-9-]+\\.)+[A-Z]{2,}$"
    in
    email
        |> andThen
            (\email ->
                if Regex.contains format email then
                    succeed email

                else
                    fail (value InvalidEmail)
            )


phone : Form.Validate.Validation ValidationError String
phone =
    let
        validate str =
            let
                phoneNo =
                    String.trim str
            in
            if String.length phoneNo >= 7 then
                Ok phoneNo

            else
                Err (customError InvalidPhone)
    in
    customValidation string validate


isotime : Form.Validate.Validation ValidationError String
isotime =
    let
        validate str =
            if Regex.contains isoTimeFormat str then
                Ok (str ++ ":00")

            else
                Err (customError (InvalidISOTime "Invalid format"))
    in
    customValidation string validate


isoDateFormat : Regex.Regex
isoDateFormat =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^(19|20)[0-9]{2}-[0-9]{2}-[0-9]{2}$"


isoTimeFormat : Regex.Regex
isoTimeFormat =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^[0-9]{2}:[0-9]{2}(:[0-9]{2})?$"


set :
    List String
    -> Form.Validate.Validation ValidationError (Set.Set String)
set values =
    let
        field value =
            Form.Validate.field value bool
                |> Form.Validate.map (\v -> ( v, value ))
    in
    List.map field values
        |> Form.Validate.sequence
        |> Form.Validate.map (\list -> List.filter Tuple.first list)
        |> Form.Validate.map (\list -> List.map Tuple.second list)
        |> Form.Validate.map Set.fromList


nonEmptySet :
    List String
    -> Form.Validate.Validation ValidationError (Set.Set String)
nonEmptySet values =
    let
        validate s =
            if Set.isEmpty s then
                Err (customError EmptySet)

            else
                Ok s
    in
    customValidation (set values) validate


nonEmptyList : List a -> Form.Validate.Validation ValidationError (List a)
nonEmptyList lst field =
    if List.isEmpty lst then
        Err (customError EmptyList)

    else
        Ok lst


termsAgreement : Validation ValidationError Bool
termsAgreement =
    let
        validate terms =
            case terms of
                False ->
                    Err (customError AcceptTerms)

                True ->
                    Ok True
    in
    customValidation bool validate
