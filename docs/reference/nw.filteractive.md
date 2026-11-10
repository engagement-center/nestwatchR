# Filter Nesting Attempts to the Active Nesting Period

Trimming nesting attempt checks to the active periods (nest contains or
likely contains host or brood parasite eggs and/or chicks) may provide
better pictures of nest start and end dates. Knowing nest active dates
can be useful when calculating nest exposure days as well as comparing
changes in the nesting period over space and time. This function notes
individual nest checks within an attempt if no eggs or chicks of either
host or parasite species are recorded (in numbers, as present, or likely
present (i.e. an adult is on the nest and was not flushed)). This
differs slightly from
[nw.filterphenology](https://engagement-center.github.io/nestwatchR/reference/nw.filterphenology.md)'s
`trim_to_active = T` argument where build is considered active in the
whole nest phenologic cycle and automatically included the fail/fledge
check where the first 0 eggs/young are recorded after being positive.
See details below.

## Usage

``` r
nw.filteractive(data, mode, include_ffcheck = FALSE, output = NULL)
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

- include_ffcheck:

  logical; TRUE or FALSE indicating whether the fledge/fail check (first
  0 eggs/young recorded after last positive count) should be retained
  (not flagged or removed). Though the nest is inactive, retaining this
  may allow more estimation of fledge/fail dates closer to the true,
  unobserved date. Defaults set to FALSE.

- output:

  character; An optional character string to custom name the output
  dataframe.

## Value

dataframe

## Details

This function summarizes by nest check, if eggs or chicks were counted
or present, or if there is evidence of adult behavior indicative of eggs
or chicks being present but not visible in the nest. If evidence of eggs
or chicks are not present, the check is noted as not active. This
function then flags (or removes) the leading and trailing checks noted
as inactive. If a check is noted as inactive, but occur between two
active checks, that check will not be flagged – this usually occurs when
a participant has not recorded full data for that check, like in cases
where an adult was present but eggs/chicks were not looked for. This
method flags (or removes) the last "fail/fledge check", as the contents
of the nest in that case are usually 0. This function also does not
account for nests being failed by abandonment or partial predation, as
there would still be a record of eggs being present despite the nest
having failed (knowingly or unknowingly by the participant). This
function contrasts with
[nw.filterphenology](https://engagement-center.github.io/nestwatchR/reference/nw.filterphenology.md)'s
`trim_to_active = T` argument, where in the latter only host eggs/young
are considered, the "fail/fledge" check (first 0 egg/chick count after a
positive count) is retained automatically, checks prior to eggs/young
are retained if a nest is incomplete or complete (suspected to be in
build), and a species-specific phenology table is used to identify
period of unacceptable length (such as long in incubation may suggest
abandonment).

## Examples

``` r
# Simplified NestWatch dataset with nest summary dates
# Checks 1 & 5 should be flagged as inactive (no eggs/young/or adults on the nest)
data <- data.frame(Attempt.ID = c("A1", "A1", "A1", "A1", "A1"),
                  Species.Code = rep("carwre", 5),
                  # simulate data for four visits to one nest
                  Visit.Datetime = c(as.POSIXct(rep(NA, 5))),
                  Visit.ID = c("S001", "S002", "S003", "S004", "S005"),
                  Visit.Datetime = as.POSIXct(c("2024-05-01", "2024-05-05",
                                                "2024-05-08", "2024-05-15", "2024-05-18")),
                  Adult.Activity = c(NA, NA, NA, NA, NA),
                  Host.Eggs.Count = c(0, 2, 0, 2, 0),
                  Host.Eggs.Present.Uncounted = rep(NA, 5),
                  Brood.Parasite.Eggs.Count = c(0, 0, NA, 0, 0),
                  Brood.Parasite.Eggs.Present.Uncounted = c(0, 0, NA, 0, 0),
                  Live.Host.Young.Present.Uncounted = rep(NA, 5),
                  Live.Host.Young.Count = c(0, 0, NA, 0, 0),
                  Brood.Parasite.Live.Young.Count = c(0, 0, NA, 0, 0),
                  Brood.Parasite.Live.Young.Present.Uncounted = c(0, 0, NA, 0, 0),
                  Nest.Status = rep(NA, 5))

out <- nw.filteractive(data = data, mode = "flag")
#> ... Identified nesting attempts have been noted with 'FLAGGED' in the in column 'Flagged.Check'.
```
