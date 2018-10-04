module Tests exposing (suite)

import Shim exposing (Expectation, Test, describe, equal, test)
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as Time exposing (Interval(..), Parts)
import TimeZone


suite : Test
suite =
    describe "Time.Extra"
        [ test_partsToPosix
        , test_posixToParts
        , test_toOffset
        , test_floor
        , test_ceiling
        , test_add
        , test_diff
        , test_range
        ]


localZone : Zone
localZone =
    TimeZone.america__new_york ()


test_partsToPosix : Test
test_partsToPosix =
    let
        toTest : ( String, Parts, Parts ) -> Test
        toTest ( name, input, expected ) =
            describe name
                [ test "using local zone" <|
                    \() -> input |> Time.partsToPosix localZone |> Time.posixToParts localZone |> equal expected
                , test "using UTC" <|
                    \() -> input |> Time.partsToPosix Time.utc |> Time.posixToParts Time.utc |> equal expected
                ]
    in
    describe "partsToPosix" <|
        [ describe "is isomorphic with posixToParts when given valid parts" <|
            List.indexedMap
                (\i parts -> toTest ( String.fromInt i, parts, parts ))
                [ Parts 1969 Dec 31 23 59 59 999
                , Parts 1970 Jan 1 0 0 0 0
                , Parts 1999 Dec 31 23 59 59 999
                , Parts 2000 Jan 1 0 0 0 0
                , Parts 2008 Dec 31 20 30 40 567
                ]
        , describe "clamps each part to its upper bound" <|
            List.map toTest
                [ ( "days", Parts 2001 Feb 31 0 0 0 0, Parts 2001 Feb 28 0 0 0 0 )
                , ( "days, leap", Parts 2000 Feb 31 0 0 0 0, Parts 2000 Feb 29 0 0 0 0 )
                , ( "hours", Parts 2001 Feb 28 27 0 0 0, Parts 2001 Feb 28 23 0 0 0 )
                , ( "minutes", Parts 2001 Feb 28 0 63 0 0, Parts 2001 Feb 28 0 59 0 0 )
                , ( "seconds", Parts 2001 Feb 28 0 0 63 0, Parts 2001 Feb 28 0 0 59 0 )
                , ( "milliseconds", Parts 2001 Feb 28 0 0 0 1003, Parts 2001 Feb 28 0 0 0 999 )
                ]
        , describe "clamps each part to its lower bound" <|
            List.map toTest
                [ ( "days", Parts 2001 Feb 0 0 0 0 0, Parts 2001 Feb 1 0 0 0 0 )
                , ( "hours", Parts 2001 Feb 1 -1 0 0 0, Parts 2001 Feb 1 0 0 0 0 )
                , ( "minutes", Parts 2001 Feb 1 0 -1 0 0, Parts 2001 Feb 1 0 0 0 0 )
                , ( "seconds", Parts 2001 Feb 1 0 0 -1 0, Parts 2001 Feb 1 0 0 0 0 )
                , ( "milliseconds", Parts 2001 Feb 1 0 0 0 -1, Parts 2001 Feb 1 0 0 0 0 )
                ]
        ]


test_posixToParts : Test
test_posixToParts =
    let
        toTest : ( String, Posix, Posix ) -> Test
        toTest ( name, input, expected ) =
            describe name
                [ test "using local zone" <|
                    \() -> input |> Time.posixToParts localZone |> Time.partsToPosix localZone |> equal expected
                , test "using UTC" <|
                    \() -> input |> Time.posixToParts Time.utc |> Time.partsToPosix Time.utc |> equal expected
                ]
    in
    describe "posixToParts" <|
        [ describe "is isomorphic with partsToPosix" <|
            List.indexedMap
                (\i posix -> toTest ( String.fromInt i, posix, posix ))
                [ Time.millisToPosix 0
                , Time.millisToPosix 9999999999999
                , Time.millisToPosix 27810009000
                , Time.millisToPosix 13045607890
                , Time.millisToPosix 430010009090
                ]
        ]


test_toOffset : Test
test_toOffset =
    let
        newYork =
            TimeZone.america__new_york ()

        paris =
            TimeZone.europe__paris ()
    in
    describe "toOffset"
        [ describe "returns the offset from UTC in minutes" <|
            let
                winter =
                    Parts 2018 Jan 1 0 0 0 0

                summer =
                    Parts 2018 Jul 1 0 0 0 0

                toTest : Int -> ( Zone, Parts, Int ) -> Test
                toTest i ( zone, parts, expected ) =
                    test (String.fromInt i) <|
                        \() -> parts |> Time.partsToPosix zone |> Time.toOffset zone |> equal expected
            in
            List.indexedMap toTest
                [ ( newYork, winter, -300 )
                , ( newYork, summer, -240 )
                , ( paris, winter, 60 )
                , ( paris, summer, 120 )
                , ( Time.utc, winter, 0 )
                , ( Time.utc, summer, 0 )
                ]
        , describe "returns the correct offset at DST transitions" <|
            let
                toTest : Int -> ( Zone, Int, ( Int, Int ) ) -> Test
                toTest i ( zone, start, ( prevOffset, offset ) ) =
                    describe (String.fromInt start)
                        [ test "before start" <|
                            \() -> (start * 60000 - 1) |> Time.millisToPosix |> Time.toOffset zone |> equal prevOffset
                        , test "at start" <|
                            -- TODO
                            -- `(start + 1)` should be `start`, but we add a minute because of error in elm/time: https://github.com/elm/time/issues/7
                            -- remove once fixed
                            \() -> ((start + 1) * 60000) |> Time.millisToPosix |> Time.toOffset zone |> equal offset
                        ]
            in
            List.indexedMap toTest
                [ ( newYork, 25688520, ( -240, -300 ) )
                , ( newYork, 25345860, ( -300, -240 ) )
                ]
        ]


test_floor : Test
test_floor =
    let
        toTest : ( Interval, Parts, Parts ) -> Test
        toTest ( interval, input, expected ) =
            describe (Debug.toString interval)
                [ test "using local zone" <|
                    \() -> input |> Time.partsToPosix localZone |> Time.floor interval localZone |> Time.posixToParts localZone |> equal expected
                , test "using UTC" <|
                    \() -> input |> Time.partsToPosix Time.utc |> Time.floor interval Time.utc |> Time.posixToParts Time.utc |> equal expected
                , test "is idempotent" <|
                    \() -> input |> Time.partsToPosix localZone |> expectIdempotence (Time.floor interval localZone)
                ]
    in
    describe "floor"
        [ describe "returns the time at the previous round interval" <|
            let
                input =
                    Parts 1999 Dec 31 23 59 59 999
            in
            List.map toTest
                [ ( Millisecond, input, Parts 1999 Dec 31 23 59 59 999 )
                , ( Second, input, Parts 1999 Dec 31 23 59 59 0 )
                , ( Minute, input, Parts 1999 Dec 31 23 59 0 0 )
                , ( Hour, input, Parts 1999 Dec 31 23 0 0 0 )
                , ( Day, input, Parts 1999 Dec 31 0 0 0 0 )
                , ( Month, input, Parts 1999 Dec 1 0 0 0 0 )
                , ( Year, input, Parts 1999 Jan 1 0 0 0 0 )
                , ( Quarter, input, Parts 1999 Oct 1 0 0 0 0 )
                , ( Week, input, Parts 1999 Dec 27 0 0 0 0 )
                , ( Monday, input, Parts 1999 Dec 27 0 0 0 0 )
                , ( Tuesday, input, Parts 1999 Dec 28 0 0 0 0 )
                , ( Wednesday, input, Parts 1999 Dec 29 0 0 0 0 )
                , ( Thursday, input, Parts 1999 Dec 30 0 0 0 0 )
                , ( Friday, input, Parts 1999 Dec 31 0 0 0 0 )
                , ( Saturday, input, Parts 1999 Dec 25 0 0 0 0 )
                , ( Sunday, input, Parts 1999 Dec 26 0 0 0 0 )
                ]
        , describe "returns the same time if it's already at a round interval" <|
            let
                expectSame : a -> b -> ( b, a, a )
                expectSame a b =
                    ( b, a, a )
            in
            List.map toTest
                [ Millisecond |> expectSame (Parts 1999 Dec 31 23 59 59 999)
                , Second |> expectSame (Parts 1999 Dec 31 23 59 59 0)
                , Minute |> expectSame (Parts 1999 Dec 31 23 59 0 0)
                , Hour |> expectSame (Parts 1999 Dec 31 23 0 0 0)
                , Day |> expectSame (Parts 1999 Dec 31 0 0 0 0)
                , Month |> expectSame (Parts 1999 Dec 1 0 0 0 0)
                , Year |> expectSame (Parts 1999 Jan 1 0 0 0 0)
                , Quarter |> expectSame (Parts 1999 Oct 1 0 0 0 0)
                , Week |> expectSame (Parts 1999 Dec 27 0 0 0 0)
                , Monday |> expectSame (Parts 1999 Dec 27 0 0 0 0)
                , Tuesday |> expectSame (Parts 1999 Dec 28 0 0 0 0)
                , Wednesday |> expectSame (Parts 1999 Dec 29 0 0 0 0)
                , Thursday |> expectSame (Parts 1999 Dec 30 0 0 0 0)
                , Friday |> expectSame (Parts 1999 Dec 31 0 0 0 0)
                , Saturday |> expectSame (Parts 1999 Dec 25 0 0 0 0)
                , Sunday |> expectSame (Parts 1999 Dec 26 0 0 0 0)
                ]
        ]


test_ceiling : Test
test_ceiling =
    let
        toTest : ( Interval, Parts, Parts ) -> Test
        toTest ( interval, input, expected ) =
            describe (Debug.toString interval)
                [ test "using local zone" <|
                    \() -> input |> Time.partsToPosix localZone |> Time.ceiling interval localZone |> Time.posixToParts localZone |> equal expected
                , test "using UTC" <|
                    \() -> input |> Time.partsToPosix Time.utc |> Time.ceiling interval Time.utc |> Time.posixToParts Time.utc |> equal expected
                , test "is idempotent" <|
                    \() -> input |> Time.partsToPosix localZone |> expectIdempotence (Time.ceiling interval localZone)
                ]
    in
    describe "ceiling"
        [ describe "returns the time at the next round interval" <|
            let
                input =
                    Parts 2000 Jan 1 0 0 0 1
            in
            List.map toTest
                [ ( Millisecond, input, Parts 2000 Jan 1 0 0 0 1 )
                , ( Second, input, Parts 2000 Jan 1 0 0 1 0 )
                , ( Minute, input, Parts 2000 Jan 1 0 1 0 0 )
                , ( Hour, input, Parts 2000 Jan 1 1 0 0 0 )
                , ( Day, input, Parts 2000 Jan 2 0 0 0 0 )
                , ( Month, input, Parts 2000 Feb 1 0 0 0 0 )
                , ( Year, input, Parts 2001 Jan 1 0 0 0 0 )
                , ( Quarter, input, Parts 2000 Apr 1 0 0 0 0 )
                , ( Week, input, Parts 2000 Jan 3 0 0 0 0 )
                , ( Monday, input, Parts 2000 Jan 3 0 0 0 0 )
                , ( Tuesday, input, Parts 2000 Jan 4 0 0 0 0 )
                , ( Wednesday, input, Parts 2000 Jan 5 0 0 0 0 )
                , ( Thursday, input, Parts 2000 Jan 6 0 0 0 0 )
                , ( Friday, input, Parts 2000 Jan 7 0 0 0 0 )
                , ( Saturday, input, Parts 2000 Jan 8 0 0 0 0 )
                , ( Sunday, input, Parts 2000 Jan 2 0 0 0 0 )
                ]
        , describe "returns the same time if it's already at a round interval" <|
            let
                expectSame : a -> b -> ( b, a, a )
                expectSame a b =
                    ( b, a, a )
            in
            List.map toTest
                [ Millisecond |> expectSame (Parts 2000 Jan 1 0 0 0 1)
                , Second |> expectSame (Parts 2000 Jan 1 0 0 1 0)
                , Minute |> expectSame (Parts 2000 Jan 1 0 1 0 0)
                , Hour |> expectSame (Parts 2000 Jan 1 1 0 0 0)
                , Day |> expectSame (Parts 2000 Jan 2 0 0 0 0)
                , Month |> expectSame (Parts 2000 Feb 1 0 0 0 0)
                , Year |> expectSame (Parts 2001 Jan 1 0 0 0 0)
                , Quarter |> expectSame (Parts 2000 Apr 1 0 0 0 0)
                , Week |> expectSame (Parts 2000 Jan 3 0 0 0 0)
                , Monday |> expectSame (Parts 2000 Jan 3 0 0 0 0)
                , Tuesday |> expectSame (Parts 2000 Jan 4 0 0 0 0)
                , Wednesday |> expectSame (Parts 2000 Jan 5 0 0 0 0)
                , Thursday |> expectSame (Parts 2000 Jan 6 0 0 0 0)
                , Friday |> expectSame (Parts 2000 Jan 7 0 0 0 0)
                , Saturday |> expectSame (Parts 2000 Jan 8 0 0 0 0)
                , Sunday |> expectSame (Parts 2000 Jan 2 0 0 0 0)
                ]
        ]


intervals : List Interval
intervals =
    [ Millisecond
    , Second
    , Minute
    , Hour
    , Day
    , Month
    , Year
    , Quarter
    , Week
    , Monday
    , Tuesday
    , Wednesday
    , Thursday
    , Friday
    , Saturday
    , Sunday
    ]


test_add : Test
test_add =
    let
        toTest : String -> (Zone -> Posix -> Posix) -> Parts -> Parts -> Test
        toTest name f input expected =
            describe name
                [ test "using local zone" <|
                    \() -> input |> Time.partsToPosix localZone |> f localZone |> Time.posixToParts localZone |> equal expected
                , test "using UTC" <|
                    \() -> input |> Time.partsToPosix Time.utc |> f Time.utc |> Time.posixToParts Time.utc |> equal expected
                ]
    in
    describe "add"
        [ describe "add 0 x == x" <|
            cross
                (\interval parts ->
                    toTest (Debug.toString interval) (Time.add interval 0) parts parts
                )
                intervals
                [ Parts 1999 Dec 31 23 59 59 999
                , Parts 2000 Jan 1 0 0 0 0
                , Parts 2000 Jul 15 12 30 30 500
                ]
        , describe "add -n (add n x) == x" <|
            cross
                (\interval parts ->
                    toTest (Debug.toString interval) (\zone -> Time.add interval 5 zone >> Time.add interval -5 zone) parts parts
                )
                -- note: not always true for adding Month, Quarter, or Year intervals, as month and year lengths are not consistent
                (intervals |> List.filter (\i -> not (i == Month || i == Quarter || i == Year)))
                [ Parts 1999 Dec 31 23 59 59 999
                , Parts 2000 Jan 1 0 0 0 0
                , Parts 2000 Jul 15 12 30 30 500
                ]
        , describe "adding positive numbers works as expected" <|
            List.map
                (\( interval, n, expected ) ->
                    toTest (Debug.toString ( interval, n )) (Time.add interval n) (Parts 1999 Dec 31 23 59 59 999) expected
                )
                [ ( Millisecond, 500, Parts 2000 Jan 1 0 0 0 499 )
                , ( Millisecond, 1500, Parts 2000 Jan 1 0 0 1 499 )
                , ( Second, 30, Parts 2000 Jan 1 0 0 29 999 )
                , ( Second, 90, Parts 2000 Jan 1 0 1 29 999 )
                , ( Minute, 30, Parts 2000 Jan 1 0 29 59 999 )
                , ( Minute, 90, Parts 2000 Jan 1 1 29 59 999 )
                , ( Hour, 12, Parts 2000 Jan 1 11 59 59 999 )
                , ( Hour, 36, Parts 2000 Jan 2 11 59 59 999 )
                , ( Day, 15, Parts 2000 Jan 15 23 59 59 999 )
                , ( Day, 60, Parts 2000 Feb 29 23 59 59 999 )
                , ( Month, 1, Parts 2000 Jan 31 23 59 59 999 )
                , ( Month, 2, Parts 2000 Feb 29 23 59 59 999 )
                , ( Month, 4, Parts 2000 Apr 30 23 59 59 999 )
                , ( Month, 14, Parts 2001 Feb 28 23 59 59 999 )
                , ( Quarter, 1, Parts 2000 Mar 31 23 59 59 999 )
                , ( Quarter, 3, Parts 2000 Sep 30 23 59 59 999 )
                , ( Year, 5, Parts 2004 Dec 31 23 59 59 999 )
                , ( Week, 8, Parts 2000 Feb 25 23 59 59 999 )
                ]
        ]


test_diff : Test
test_diff =
    let
        toTest : String -> Parts -> Parts -> Int -> Interval -> Test
        toTest name parts1 parts2 expected interval =
            describe name
                [ test "using local zone" <|
                    \() -> Time.diff interval localZone (parts1 |> Time.partsToPosix localZone) (parts2 |> Time.partsToPosix localZone) |> equal expected
                , test "using UTC" <|
                    \() -> Time.diff interval Time.utc (parts1 |> Time.partsToPosix Time.utc) (parts2 |> Time.partsToPosix Time.utc) |> equal expected
                ]
    in
    describe "diff"
        [ describe "diff x x == 0" <|
            cross
                (\interval parts ->
                    toTest (Debug.toString ( interval, parts )) parts parts 0 interval
                )
                intervals
                [ Parts 1999 Dec 31 23 59 59 999
                , Parts 2001 Jan 1 0 0 0 0
                ]
        , describe "diff a b == -(diff b a)" <|
            let
                parts1 =
                    Parts 1999 Dec 31 23 59 59 999
            in
            List.map
                (\( parts2, expected, interval ) ->
                    describe (Debug.toString ( interval, expected ))
                        [ toTest "`diff earlier later` returns positive numbers" parts1 parts2 expected interval
                        , toTest "`diff later earlier` returns negative numbers" parts2 parts1 -expected interval
                        ]
                )
                [ ( Parts 2000 Jan 1 0 0 0 499, 500, Millisecond )
                , ( Parts 2000 Jan 1 0 0 1 499, 1500, Millisecond )
                , ( Parts 2000 Jan 1 0 0 29 999, 30, Second )
                , ( Parts 2000 Jan 1 0 1 29 999, 90, Second )
                , ( Parts 2000 Jan 1 0 29 59 999, 30, Minute )
                , ( Parts 2000 Jan 1 1 29 59 999, 90, Minute )
                , ( Parts 2000 Jan 1 11 59 59 999, 12, Hour )
                , ( Parts 2000 Jan 2 11 59 59 999, 36, Hour )
                , ( Parts 2000 Jan 15 23 59 59 999, 15, Day )
                , ( Parts 2000 Feb 29 23 59 59 999, 60, Day )
                , ( Parts 2000 Jan 31 23 59 59 999, 1, Month )
                , ( Parts 2000 Feb 29 23 59 59 999, 1, Month )
                , ( Parts 2000 Apr 30 23 59 59 999, 3, Month )
                , ( Parts 2001 Feb 28 23 59 59 999, 13, Month )
                , ( Parts 2000 Mar 31 23 59 59 999, 1, Quarter )
                , ( Parts 2000 Sep 30 23 59 59 999, 2, Quarter )
                , ( Parts 2004 Dec 31 23 59 59 999, 5, Year )
                , ( Parts 2000 Feb 25 23 59 59 999, 8, Week )
                , ( Parts 2000 Jan 11 0 0 0 0, 2, Monday )
                , ( Parts 2000 Jan 11 0 0 0 0, 2, Tuesday )
                , ( Parts 2000 Jan 11 0 0 0 0, 1, Wednesday )
                , ( Parts 2000 Jan 11 0 0 0 0, 1, Thursday )
                , ( Parts 2000 Jan 11 0 0 0 0, 1, Friday )
                , ( Parts 2000 Jan 11 0 0 0 0, 2, Saturday )
                , ( Parts 2000 Jan 11 0 0 0 0, 2, Sunday )
                ]
        , describe "diffing Years returns a number of whole years as determined by calendar date (anniversary)"
            [ toTest "1" (Parts 2000 Feb 29 12 30 30 500) (Parts 2001 Feb 28 12 30 30 500) 0 Year
            , toTest "2" (Parts 2000 Feb 29 12 30 30 500) (Parts 2004 Feb 29 12 30 30 500) 4 Year
            ]
        , describe "diffing Months returns a number of whole months as determined by calendar date"
            [ toTest "1" (Parts 2000 Jan 31 0 0 0 0) (Parts 2000 Feb 29 0 0 0 0) 0 Month
            , toTest "2" (Parts 2000 Jan 31 0 0 0 0) (Parts 2000 Mar 31 0 0 0 0) 2 Month
            , toTest "3" (Parts 2000 Jan 31 0 0 0 0) (Parts 2000 Apr 30 0 0 0 0) 2 Month
            , toTest "4" (Parts 2000 Jan 31 0 0 0 0) (Parts 2001 Feb 28 0 0 0 0) 12 Month
            ]
        ]


test_range : Test
test_range =
    let
        toTest : Interval -> Int -> Parts -> Parts -> List Parts -> Test
        toTest interval step start until expected =
            describe ([ Debug.toString interval, String.fromInt step ] |> String.join " ")
                [ test "using local zone" <|
                    \() ->
                        Time.range interval step localZone (Time.partsToPosix localZone start) (Time.partsToPosix localZone until)
                            |> List.map (Time.posixToParts localZone)
                            |> equal expected
                , test "using UTC" <|
                    \() ->
                        Time.range interval step Time.utc (Time.partsToPosix Time.utc start) (Time.partsToPosix Time.utc until)
                            |> List.map (Time.posixToParts Time.utc)
                            |> equal expected
                ]
    in
    describe "range"
        [ toTest Millisecond 200 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Jan 1 0 0 0 600) <|
            [ Parts 2000 Jan 1 0 0 0 0
            , Parts 2000 Jan 1 0 0 0 200
            , Parts 2000 Jan 1 0 0 0 400
            ]
        , toTest Second 30 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Jan 1 0 1 30 0) <|
            [ Parts 2000 Jan 1 0 0 0 0
            , Parts 2000 Jan 1 0 0 30 0
            , Parts 2000 Jan 1 0 1 0 0
            ]
        , toTest Minute 45 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Jan 1 2 15 0 0) <|
            [ Parts 2000 Jan 1 0 0 0 0
            , Parts 2000 Jan 1 0 45 0 0
            , Parts 2000 Jan 1 1 30 0 0
            ]
        , toTest Hour 18 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Jan 3 6 0 0 0) <|
            [ Parts 2000 Jan 1 0 0 0 0
            , Parts 2000 Jan 1 18 0 0 0
            , Parts 2000 Jan 2 12 0 0 0
            ]
        , toTest Day 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Jan 7 0 0 0 0) <|
            [ Parts 2000 Jan 1 0 0 0 0
            , Parts 2000 Jan 3 0 0 0 0
            , Parts 2000 Jan 5 0 0 0 0
            ]
        , toTest Month 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Jul 1 0 0 0 0) <|
            [ Parts 2000 Jan 1 0 0 0 0
            , Parts 2000 Mar 1 0 0 0 0
            , Parts 2000 May 1 0 0 0 0
            ]
        , toTest Year 10 (Parts 2000 Jan 1 0 0 0 0) (Parts 2030 Jan 1 0 0 0 0) <|
            [ Parts 2000 Jan 1 0 0 0 0
            , Parts 2010 Jan 1 0 0 0 0
            , Parts 2020 Jan 1 0 0 0 0
            ]
        , toTest Quarter 1 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Sep 1 0 0 0 0) <|
            [ Parts 2000 Jan 1 0 0 0 0
            , Parts 2000 Apr 1 0 0 0 0
            , Parts 2000 Jul 1 0 0 0 0
            ]
        , toTest Week 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Feb 14 0 0 0 0) <|
            [ Parts 2000 Jan 3 0 0 0 0
            , Parts 2000 Jan 17 0 0 0 0
            , Parts 2000 Jan 31 0 0 0 0
            ]
        , toTest Monday 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Feb 14 0 0 0 0) <|
            [ Parts 2000 Jan 3 0 0 0 0
            , Parts 2000 Jan 17 0 0 0 0
            , Parts 2000 Jan 31 0 0 0 0
            ]
        , toTest Tuesday 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Feb 15 0 0 0 0) <|
            [ Parts 2000 Jan 4 0 0 0 0
            , Parts 2000 Jan 18 0 0 0 0
            , Parts 2000 Feb 1 0 0 0 0
            ]
        , toTest Wednesday 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Feb 16 0 0 0 0) <|
            [ Parts 2000 Jan 5 0 0 0 0
            , Parts 2000 Jan 19 0 0 0 0
            , Parts 2000 Feb 2 0 0 0 0
            ]
        , toTest Thursday 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Feb 17 0 0 0 0) <|
            [ Parts 2000 Jan 6 0 0 0 0
            , Parts 2000 Jan 20 0 0 0 0
            , Parts 2000 Feb 3 0 0 0 0
            ]
        , toTest Friday 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Feb 18 0 0 0 0) <|
            [ Parts 2000 Jan 7 0 0 0 0
            , Parts 2000 Jan 21 0 0 0 0
            , Parts 2000 Feb 4 0 0 0 0
            ]
        , toTest Saturday 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Feb 12 0 0 0 0) <|
            [ Parts 2000 Jan 1 0 0 0 0
            , Parts 2000 Jan 15 0 0 0 0
            , Parts 2000 Jan 29 0 0 0 0
            ]
        , toTest Sunday 2 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Feb 13 0 0 0 0) <|
            [ Parts 2000 Jan 2 0 0 0 0
            , Parts 2000 Jan 16 0 0 0 0
            , Parts 2000 Jan 30 0 0 0 0
            ]
        , describe "example"
            [ toTest Day 2 (Parts 2007 Mar 15 11 55 0 0) (Parts 2007 Mar 22 0 0 0 0) <|
                [ Parts 2007 Mar 16 0 0 0 0
                , Parts 2007 Mar 18 0 0 0 0
                , Parts 2007 Mar 20 0 0 0 0
                ]
            ]
        , describe "begins with the interval nearest to start date"
            [ toTest Day 10 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Jan 30 0 0 0 0) <|
                [ Parts 2000 Jan 1 0 0 0 0
                , Parts 2000 Jan 11 0 0 0 0
                , Parts 2000 Jan 21 0 0 0 0
                ]
            , toTest Day 10 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Jan 31 0 0 0 0) <|
                [ Parts 2000 Jan 1 0 0 0 0
                , Parts 2000 Jan 11 0 0 0 0
                , Parts 2000 Jan 21 0 0 0 0
                ]
            , toTest Day 10 (Parts 2000 Jan 1 0 0 0 0) (Parts 2000 Feb 1 0 0 0 0) <|
                [ Parts 2000 Jan 1 0 0 0 0
                , Parts 2000 Jan 11 0 0 0 0
                , Parts 2000 Jan 21 0 0 0 0
                , Parts 2000 Jan 31 0 0 0 0
                ]
            ]
        , test "can return the empty list" <|
            \() ->
                let
                    posix =
                        Parts 2020 Jan 1 0 0 0 0 |> Time.partsToPosix Time.utc
                in
                Time.range Millisecond 1 Time.utc posix posix |> equal []
        , describe "can return a large list (tail recursion)"
            [ let
                start =
                    Parts 1950 Jan 1 0 0 0 0 |> Time.partsToPosix Time.utc

                until =
                    Parts 2050 Jan 1 0 0 0 0 |> Time.partsToPosix Time.utc

                expectedLength =
                    Time.diff Day Time.utc start until
              in
              test ("length: " ++ String.fromInt expectedLength) <|
                \() -> Time.range Day 1 Time.utc start until |> List.length |> equal expectedLength
            ]
        ]



-- list


cross : (a -> b -> c) -> List a -> List b -> List c
cross f xs ys =
    List.concatMap (\x -> List.map (f x) ys) xs



-- expectation


expectIsomorphism : (x -> y) -> (y -> x) -> x -> Expectation
expectIsomorphism xToY yToX x =
    x |> xToY |> yToX |> equal x


expectIdempotence : (x -> x) -> x -> Expectation
expectIdempotence f x =
    f (f x) |> equal (f x)
