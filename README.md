# time-extra

Extra functions for working with `Posix` times from [`elm/time`][elmtime].

[elmtime]: https://package.elm-lang.org/packages/elm/time/latest/


## Overview

- Create `Posix` times: [`Parts`][Parts], [`partsToPosix`][partsToPosix]
- Diff between times: [`diff`][diff]
- Shift times: [`add`][add], [`floor`][floor], [`ceiling`][ceiling]
- Create lists: [`range`][range]
- Time zone offsets: [`toOffset`][toOffset]

[Parts]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra#Parts
[partsToPosix]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra#partsToPosix
[diff]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra#diff
[add]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra#add
[floor]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra#floor
[ceiling]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra#ceiling
[range]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra#range
[toOffset]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra#toOffset


## Examples

See [the docs][docs] for the full API.

[docs]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra


### Create a Posix time

```elm
import Time exposing (Month(..), utc)
import Time.Extra as Time


Time.Parts 2018 Sep 26 14 30 0 0 |> Time.partsToPosix utc
```


### Find the difference between two Posix times

```elm
import Time exposing (Month(..), utc)
import Time.Extra as Time exposing (Interval(..))


time1 = Time.Parts 2020 Sep 1 12 0 0 0 |> Time.partsToPosix utc
time2 = Time.Parts 2020 Sep 4 11 0 0 0 |> Time.partsToPosix utc

Time.diff Day utc time1 time2
    == 2
```


### Create a list of Posix times

```elm
import Time exposing (Month(..), utc)
import Time.Extra as Time exposing (Interval(..))


start = Time.Parts 2020 Jan 1 12 0 0 0 |> Time.partsToPosix utc
until = start |> Time.add Day 1 utc

Time.range Hour 4 utc start until
    == List.map (Time.partsToPosix utc)
        [ Time.Parts 2020 Jan 1 12 0 0 0
        , Time.Parts 2020 Jan 1 16 0 0 0
        , Time.Parts 2020 Jan 1 20 0 0 0
        , Time.Parts 2020 Jan 2 0 0 0 0
        , Time.Parts 2020 Jan 2 4 0 0 0
        , Time.Parts 2020 Jan 2 8 0 0 0
        ]
```
