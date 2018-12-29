module Form.Builder.Error exposing
    ( Msg(..)
    , ValidationError(..)
    , ValidationErrorPersonName(..)
    , errorString
    )

import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))


type ValidationError
    = InvalidISODate String
    | InvalidISOTime String
    | EmptyList
    | EmptySet
    | InvalidSet
    | AcceptTerms
    | InvalidPersonName ValidationErrorPersonName
    | InvalidPhone


type Msg
    = FormMsg Form.Msg


type ValidationErrorPersonName
    = LastNameMissing
    | NameTooShort


errorString : ErrorValue ValidationError -> String
errorString error =
    case error of
        Empty ->
            "Fältet får inte vara tomt."

        InvalidString ->
            "Du måste fylla i fältet."

        InvalidEmail ->
            "Det är inte en giltig e-postadress."

        InvalidFormat ->
            "Fältet har inte rätt format."

        InvalidInt ->
            "Det är inte en siffra."

        InvalidFloat ->
            "Det är inte ett flyttal."

        InvalidBool ->
            "Du måste svara ja eller nej."

        SmallerIntThan n ->
            "Får inte vara större än " ++ String.fromInt n ++ "."

        GreaterIntThan n ->
            "Får inte vara mindre än " ++ String.fromInt n ++ "."

        SmallerFloatThan n ->
            "Får inte vara större än " ++ String.fromFloat n ++ "."

        GreaterFloatThan n ->
            "Får inte vara mindre än " ++ String.fromFloat n ++ "."

        ShorterStringThan n ->
            "Måste vara minst " ++ String.fromInt n ++ " tecken."

        LongerStringThan n ->
            "Får inte vara längre än " ++ String.fromInt n ++ " tecken."

        NotIncludedIn ->
            "Är inte ett korrekt val från listan."

        CustomError (InvalidISODate _) ->
            "Det är inte ett giltigt datum."

        CustomError (InvalidISOTime _) ->
            "Det är inte ett giltig klockslag. Behöver vara i formatet hh:mm - t.ex 15:30."

        CustomError EmptyList ->
            "Du måste lägga till minst en sak i listan."

        CustomError EmptySet ->
            "Du måste göra minst ett val."

        CustomError InvalidSet ->
            "Den kombination av val du har gjort är inte tillåten."

        CustomError AcceptTerms ->
            "Du måste acceptera användarvillkoren."

        CustomError (InvalidPersonName LastNameMissing) ->
            "Du behöver ange både för- och efternamn."

        CustomError (InvalidPersonName NameTooShort) ->
            "Namnet behöver bestå av minst 3 tecken."

        CustomError InvalidPhone ->
            "Telefonnumret behöver bestå av minst 8 siffror."
