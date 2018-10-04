module Time.Extra exposing
    ( Parts, partsToPosix
    , posixToParts
    , Interval(..)
    , diff
    , add, floor, ceiling
    , range
    , toOffset
    )

{-|


# Create

@docs Parts, partsToPosix


# Convert

@docs posixToParts


# Intervals

@docs Interval


## Difference

@docs diff


## Shift

@docs add, floor, ceiling


## Lists

@docs range


# Time zone offset

@docs toOffset

-}

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


{-| What is the offset from UTC, in minutes, for this `Zone` at this
`Posix` time?

    import Time exposing (Month(..))
    import Time.Extra exposing (Parts, partsToPosix, toOffset)

    toOffset nyc
        (partsToPosix nyc (Parts 2018 Sep 26 10 30 0 0))
        == -240

    -- assuming `nyc` is a `Zone` for America/New_York

**Note:** It's possible to verify the example above by using time zone data
from the package [justinmimbs/timezone-data][tzdata] to define `nyc`:

    import TimeZone

    nyc =
        TimeZone.america__new_york ()

[tzdata]: https://package.elm-lang.org/packages/justinmimbs/timezone-data/latest/

-}
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


{-| Represents a time abstractly by describing its calendar date and clock
time. Given a time zone, this information can be converted to a `Posix`, i.e.
a concrete moment in time.
-}
type alias Parts =
    { year : Int
    , month : Month
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    }


{-| Create a `Posix` from a description of a time and a specific time zone.

    import Time exposing (Month(..), utc)
    import Time.Extra exposing (Parts, partsToPosix)

    partsToPosix utc (Parts 2018 Sep 26 14 30 0 0)

Any out-of-range day or time values will be clamped within range.

    partsToPosix utc (Parts 2018 Sep 31 24 60 -60 -500)
        == partsToPosix utc (Parts 2018 Sep 30 23 59 0 0)

-}
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


{-| Convert a `Posix` to a description of a time in a specific time zone. This
is a convenience function for extracting parts of a time into a single record.

    import Time exposing (Month(..), utc)
    import Time.Extra exposing (Parts, partsToPosix, posixToParts)

    posixToParts
        utc
        (partsToPosix utc (Parts 2018 Sep 26 14 30 0 0))
        == { year = 2018
           , month = Sep
           , day = 26
           , hour = 14
           , minute = 30
           , second = 0
           , millisecond = 0
           }

-}
posixToParts : Zone -> Posix -> Parts
posixToParts zone posix =
    { year = toYear zone posix
    , month = toMonth zone posix
    , day = toDay zone posix
    , hour = toHour zone posix
    , minute = toMinute zone posix
    , second = toSecond zone posix
    , millisecond = toMillis zone posix
    }


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


{-| Shift a time into the past or future by adding a number of whole intervals.

    import Time exposing (Month(..), utc)
    import Time.Extra exposing (Interval(..), Parts, partsToPosix, add)

    add Week 2 utc (partsToPosix utc (Parts 2018 Sep 1 11 55 0 0))
        == partsToPosix utc (Parts 2018 Sep 15 11 55 0 0)

When adding `Month`, `Quarter`, or `Year` intervals, day values are clamped to
the end of the month if necessary.

    add Month 1 utc (partsToPosix utc (Parts 2020 Jan 31 0 0 0 0))
        == partsToPosix utc (Parts 2020 Feb 29 0 0 0 0)

-}
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


{-| Get the difference, as a number of whole intervals, between two times.

    import Time exposing (Month(..), utc)
    import Time.Extra exposing (Interval(..), Parts, partsToPosix, diff)

    diff Month utc
        (partsToPosix utc (Parts 2020 Jan 2 0 0 0 0))
        (partsToPosix utc (Parts 2020 Apr 1 0 0 0 0))
        == 2

-}
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


{-| Round down a time to the beginning of the closest interval. The resulting
time will be less than or equal to the one provided.

    import Time exposing (Month(..), utc)
    import Time.Extra exposing (Interval(..), Parts, partsToPosix, floor)

    floor Hour utc
        (partsToPosix utc (Parts 1999 Dec 31 23 59 59 999))
        == (partsToPosix utc (Parts 1999 Dec 31 23 0 0 0))

-}
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


{-| Round up a time to the beginning of the closest interval. The resulting
time will be greater than or equal to the one provided.

    import Time exposing (Month(..), utc)
    import Time.Extra exposing (Interval(..), Parts, partsToPosix, ceiling)

    ceiling Hour utc
        (partsToPosix utc (Parts 1999 Dec 31 23 59 59 999))
        == (partsToPosix utc (Parts 2000 Jan 1 0 0 0 0))

-}
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


{-| Create a list of times, at rounded intervals, increasing by a step value,
between two times. The list will start on or after the first time, and end
before the second time.

    import Time exposing (Month(..), utc)
    import Time.Extra exposing (Interval(..), Parts, partsToPosix, range)

    start = Parts 2020 Jan 1 12 0 0 0
    until = Parts 2020 Jan 8 0 0 0 0

    range Day 2 utc (partsToPosix utc start) (partsToPosix utc until)
        == List.map (partsToPosix utc)
            [ Parts 2020 Jan 2 0 0 0 0
            , Parts 2020 Jan 4 0 0 0 0
            , Parts 2020 Jan 6 0 0 0 0
            ]

-}
range : Interval -> Int -> Zone -> Posix -> Posix -> List Posix
range interval step zone start until =
    rangeHelp interval (max 1 step) zone until [] (start |> ceiling interval zone)


rangeHelp : Interval -> Int -> Zone -> Posix -> List Posix -> Posix -> List Posix
rangeHelp interval step zone until revList current =
    if posixToMillis current < posixToMillis until then
        rangeHelp interval step zone until (current :: revList) (current |> add interval step zone)

    else
        List.reverse revList
