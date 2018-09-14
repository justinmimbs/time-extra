module Time.Extra exposing (Interval(..), Parts, add, ceiling, diff, floor, partsToPosix, range, toOffset)

import Date exposing (Date)
import Time exposing (..)


dateToMillis : Date -> Int
dateToMillis date =
    let
        daysSinceEpoch =
            (date |> Date.toRataDie) - 719163
    in
    daysSinceEpoch * 86400000


timeFromClock : Int -> Int -> Int -> Int -> Int
timeFromClock hour minute second millisecond =
    hour * 3600000 + minute * 60000 + second * 1000 + millisecond


timeFromPosix : Zone -> Posix -> Int
timeFromPosix zone posix =
    timeFromClock
        (toHour zone posix)
        (toMinute zone posix)
        (toSecond zone posix)
        (toMillis zone posix)


toOffset : Zone -> Posix -> Int
toOffset zone posix =
    let
        localMillis =
            (Date.fromPosix zone posix |> dateToMillis)
                + timeFromPosix zone posix

        millis =
            posixToMillis posix
    in
    (localMillis - millis) // 60000


posixFromDateTime : Zone -> Date -> Int -> Posix
posixFromDateTime zone date time =
    -- find the local offset
    let
        millis =
            (date |> dateToMillis) + time

        offset0 =
            millis |> millisToPosix |> toOffset zone

        posix1 =
            (millis - offset0 * 60000) |> millisToPosix

        offset1 =
            posix1 |> toOffset zone
    in
    if offset0 == offset1 then
        posix1

    else
        -- local offset has changed within `offset0` time period (e.g. DST switch)
        let
            posix2 =
                (millis - offset1 * 60000) |> millisToPosix

            offset2 =
                posix2 |> toOffset zone
        in
        if offset1 == offset2 then
            posix2

        else
            -- `millis` is within the lost hour of a local switch
            posix1


type alias Parts =
    { year : Int
    , month : Month
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    }


partsToPosix : Zone -> Parts -> Posix
partsToPosix zone { year, month, day, hour, minute, second, millisecond } =
    posixFromDateTime zone
        (Date.fromCalendarDate year month day)
        (timeFromClock
            (hour |> clamp 0 23)
            (minute |> clamp 0 59)
            (second |> clamp 0 59)
            (millisecond |> clamp 0 999)
        )


{-| Represents an interval of time.
-}
type Interval
    = Year
    | Quarter
    | Month
    | Week
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    | Day
    | Hour
    | Minute
    | Second
    | Millisecond


add : Interval -> Int -> Zone -> Posix -> Posix
add interval n zone posix =
    case interval of
        Millisecond ->
            (posix |> posixToMillis) + n |> millisToPosix

        Second ->
            add Millisecond (n * 1000) zone posix

        Minute ->
            add Millisecond (n * 60000) zone posix

        Hour ->
            add Millisecond (n * 3600000) zone posix

        Day ->
            posixFromDateTime zone
                (Date.fromPosix zone posix |> Date.add Date.Days n)
                (timeFromPosix zone posix)

        Month ->
            posixFromDateTime zone
                (Date.fromPosix zone posix |> Date.add Date.Months n)
                (timeFromPosix zone posix)

        Year ->
            add Month (n * 12) zone posix

        Quarter ->
            add Month (n * 3) zone posix

        Week ->
            add Day (n * 7) zone posix

        weekday ->
            add Day (n * 7) zone posix


toFractionalDay : Zone -> Posix -> Float
toFractionalDay zone posix =
    (timeFromPosix zone posix |> toFloat) / 86400000


toRataDieMoment : Zone -> Posix -> Float
toRataDieMoment zone posix =
    (Date.fromPosix zone posix |> Date.toRataDie |> toFloat)
        + toFractionalDay zone posix


{-| The number of whole months between date and 0001-01-01 plus fraction
representing the current month. Only used for diffing months.
-}
toMonths : Zone -> Posix -> Float
toMonths zone posix =
    let
        wholeMonths =
            (12 * (toYear zone posix - 1))
                + ((toMonth zone posix |> Date.monthToNumber) - 1)
                |> toFloat

        fractionalMonth =
            ((toDay zone posix |> toFloat) + toFractionalDay zone posix) / 100
    in
    wholeMonths + fractionalMonth


diff : Interval -> Zone -> Posix -> Posix -> Int
diff interval zone posix1 posix2 =
    case interval of
        Millisecond ->
            (posix2 |> posixToMillis) - (posix1 |> posixToMillis)

        Second ->
            diff Millisecond zone posix1 posix2 // 1000

        Minute ->
            diff Millisecond zone posix1 posix2 // 60000

        Hour ->
            diff Millisecond zone posix1 posix2 // 3600000

        Day ->
            (posix2 |> toRataDieMoment zone) - (posix1 |> toRataDieMoment zone) |> truncate

        Month ->
            (posix2 |> toMonths zone) - (posix1 |> toMonths zone) |> truncate

        Year ->
            diff Month zone posix1 posix2 // 12

        Quarter ->
            diff Month zone posix1 posix2 // 3

        Week ->
            diff Day zone posix1 posix2 // 7

        weekday ->
            diff Week zone (floor weekday zone posix1) (floor weekday zone posix2)


floor : Interval -> Zone -> Posix -> Posix
floor interval zone posix =
    case interval of
        Millisecond ->
            posix

        Second ->
            posixFromDateTime zone
                (Date.fromPosix zone posix)
                (timeFromClock (toHour zone posix) (toMinute zone posix) (toSecond zone posix) 0)

        Minute ->
            posixFromDateTime zone
                (Date.fromPosix zone posix)
                (timeFromClock (toHour zone posix) (toMinute zone posix) 0 0)

        Hour ->
            posixFromDateTime zone
                (Date.fromPosix zone posix)
                (timeFromClock (toHour zone posix) 0 0 0)

        Day ->
            floorDate Date.Day zone posix

        Month ->
            floorDate Date.Month zone posix

        Year ->
            floorDate Date.Year zone posix

        Quarter ->
            floorDate Date.Quarter zone posix

        Week ->
            floorDate Date.Week zone posix

        Monday ->
            floorDate Date.Monday zone posix

        Tuesday ->
            floorDate Date.Tuesday zone posix

        Wednesday ->
            floorDate Date.Wednesday zone posix

        Thursday ->
            floorDate Date.Thursday zone posix

        Friday ->
            floorDate Date.Friday zone posix

        Saturday ->
            floorDate Date.Saturday zone posix

        Sunday ->
            floorDate Date.Sunday zone posix


floorDate : Date.Interval -> Zone -> Posix -> Posix
floorDate dateInterval zone posix =
    posixFromDateTime zone
        (Date.fromPosix zone posix |> Date.floor dateInterval)
        0


ceiling : Interval -> Zone -> Posix -> Posix
ceiling interval zone posix =
    let
        floored =
            posix |> floor interval zone
    in
    if floored == posix then
        posix

    else
        floored |> add interval 1 zone


range : Interval -> Int -> Zone -> Posix -> Posix -> List Posix
range interval step zone start until =
    rangeHelp interval (max 1 step) zone until [] (start |> ceiling interval zone)


rangeHelp : Interval -> Int -> Zone -> Posix -> List Posix -> Posix -> List Posix
rangeHelp interval step zone until revList current =
    if posixToMillis current < posixToMillis until then
        rangeHelp interval step zone until (current :: revList) (current |> add interval step zone)

    else
        List.reverse revList
