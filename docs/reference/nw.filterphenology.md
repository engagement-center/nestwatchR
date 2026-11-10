# Filter Nesting Attempts Based on Expected Nest Phenological Periods

To provide additional checks on data quality before analysis, an analyst
may want to filter out nesting attempts which were recorded to have
nesting phenologies of unexpectedly long lengths. This function allows
an analyst to specify species-specific values for nest phenologies using
expected days in the lay, incubation, nestling, and total nesting
periods. By defining these allowable time frames an analyst can flag or
remove "run-on" nest attempts or nests which do not match the specie's
life history. This function only uses values for host eggs/young, and
does not factor in brood parasite eggs or young.

## Usage

``` r
nw.filterphenology(
  data,
  mode,
  max_phenology,
  trim_to_active = FALSE,
  output = NULL
)
```

## Arguments

- data:

  dataframe; A dataframe of merged NestWatch attempt and visits data.
  Date columns must have class `Date` and Datetime columns must have
  class `POSIXct`.

- mode:

  `"flag"` or `"remove"`; A character string defining if the analyst
  wants the identified nesting attempts to be flagged with "FLAGGED" in
  a new column, or removed from the dataset.

- max_phenology:

  dataframe; A simple dataframe with one row for each species of
  interest and the following column structure:

  - **Species**: The 6-letter species code for each species.

  - **Lay**: The number of days representing the maximum expected laying
    period for each species.

  - **Incubation**: The number of days representing the maximum expected
    incubation period (between clutch complete and hatch) for each
    species.

  - **Nestling**: The number of days representing the maximum expected
    nesting period (hatch and last fledge) for each species.

  - **Total**: The number of days representing the maximum expected
    total nesting period (spanning first initial nest building to last
    fledge) for each species.

- trim_to_active:

  logical; TRUE or FALSE indicating if nest check data should be trimmed
  to include only those rows where the nest was active (in build, lay,
  incubation, presence of live young). Build status is considered when
  Nest.Status is "in" or "cn" (for incomplete or complete nest) but no
  eggs or young have been detected yet. The first check observing a
  fledge or fail event is retained, but subsequent checks would be
  targeted for flag/removal. Flag/removal follows `mode`. Note that this
  will include cases where a nest may have been abandoned, found after
  failure, found after fledging, or other cases of inactivity. See
  [nw.filteractive](https://engagement-center.github.io/nestwatchR/reference/nw.filteractive.md)
  to isolate checks to eggs and young only.

- output:

  character; An optional character string to custom name the output
  dataframe

## Value

dataframe

## Details

This function calculates the number of days between `First.Lay.Date`,
`Hatch.Date`, and `Fledge.Date` date values, comparing each to the
user-provided acceptable date spans for each nest phase. Not all
attempts contain these summary dates. An analyst may choose to explore
[nw.estclutchsize](https://engagement-center.github.io/nestwatchR/reference/nw.estclutchsize.md),
[nw.estfirstlay](https://engagement-center.github.io/nestwatchR/reference/nw.estfirstlay.md),
[nw.esthatch](https://engagement-center.github.io/nestwatchR/reference/nw.esthatch.md),
and
[nw.estfledge](https://engagement-center.github.io/nestwatchR/reference/nw.estfledge.md)
functions to estimate these summary dates from the individual nest visit
data prior to using this function. As an additional check, this function
calculates the date span between the first occurrence of an incomplete
nest, complete nest, observed egg, or observed young and last active
date (either the last observation of eggs/live young or the first
0-egg/young count indicating fail/fledge) and compares this date span to
the user-provided "total nesting period" value.

## Examples

``` r
#' # Create phenology dataframe
phenology <- data.frame(species = c("carwre"),
                        lay = c(7),          # max observed
                        incubation = c(20),  # mean plus some extra
                        nestling = c(20),    # mean plus some extra
                        total = c(50))       # mean plus some extra

# Simplified NestWatch dataset with nest summary dates only
# Attemtp should be flagged for too long in incubation phase
data <- data.frame(Attempt.ID = c("A001"),
                   Species.Code = c("carwre"),
                   First.Lay.Date = as.Date("2026-05-23"),
                   Hatch.Date = as.Date("2026-07-14"),
                   Fledge.Date = as.Date("2026-07-30"),
                   Visit.ID = c(NA),
                   Visit.Datetime = as.POSIXct(c(NA)),
                   Nest.Status= c(NA),
                   Adult.Activity = c(NA),
                   Host.Eggs.Count = c(NA),
                   Host.Eggs.Present.Uncounted = c(NA),
                   Live.Host.Young.Count = c(NA),
                   Live.Host.Young.Present.Uncounted = c(NA))

nw.filterphenology(data = data,
                   max_phenology = phenology,
                   mode = "flag",
                   trim_to_active = FALSE)
#> ... Identified nesting attempts have been noted with 'FLAGGED' in the columns 'Flagged.Attempt' & 'Flagged.Check'.
#>   Flagged.Attempt Attempt.ID Species.Code First.Lay.Date Hatch.Date Fledge.Date
#> 1         FLAGGED       A001       carwre     2026-05-23 2026-07-14  2026-07-30
#>   Visit.ID Visit.Datetime Nest.Status Adult.Activity Host.Eggs.Count
#> 1       NA           <NA>          NA             NA              NA
#>   Host.Eggs.Present.Uncounted Live.Host.Young.Count
#> 1                          NA                    NA
#>   Live.Host.Young.Present.Uncounted
#> 1                                NA


# Simplified NestWatch dataset with nest visits data
# First and last checks should be flagged as inactive
data <- data.frame(Attempt.ID = c("A002"),
                   Species.Code = c("carwre"),
                   First.Lay.Date = as.Date("2026-05-16"),
                   Hatch.Date = as.Date("2026-05-29"),
                   Fledge.Date = as.Date("2026-06-14"),
                   Visit.ID = c(1, 2, 3, 4, 5, 6, 7),
                   Visit.Datetime = as.POSIXct(c("2026-05-05",
                         "2026-05-13", "2026-05-20", "2026-05-25",
                         "2026-06-08", "2026-06-15", "2026-06-17")),
                   Nest.Status= c("no", rep("cn", 6)),
                   Adult.Activity = rep(NA, 7),
                   Host.Eggs.Count = c(0, 0, 5, 5, 0, 0, 0),
                   Host.Eggs.Present.Uncounted = rep(NA, 7),
                  Live.Host.Young.Count = c(0, 0, 0, 0, 5, 0, 0),
                   Live.Host.Young.Present.Uncounted = rep(NA, 7))

out <- nw.filterphenology(data = data,
                          max_phenology = phenology,
                          mode = "flag",
                          trim_to_active = TRUE)
#> ... Identified nesting attempts have been noted with 'FLAGGED' in the columns 'Flagged.Attempt' & 'Flagged.Check'.
```
