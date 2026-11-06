# Estimate Fledge Date for NestWatch Data

Fledge Date data may be important for investigating relationships
between nesting success and environmental variables. These data in the
NestWatch dataset are provided by participants, and the input of these
dates may be overlooked by the participants. If not explicitly provided
by the participant, fledge date may be estimated based off hatch date or
first lay date. This function checks, in order, if the nest fledged
successfully, and if a Hatch.Date was recorded or then if a
First.Lay.Date was recorded. If these requirements are met, the function
uses user-input nest phenology time frames to estimate fledge date.
Estimated dates are denoted with a `1` in `data$Fledge.Date.Estimated`.

## Usage

``` r
nw.estfledge(data, phenology, output = NULL)
```

## Arguments

- data:

  dataframe; A dataframe containing merged NestWatch attempts and visits
  data.

- phenology:

  dataframe; A dataframe one row of phenological data for each species
  to be estimated. Data columns as follows:

  - `Species`: Species Codes for each species to be estimated.

  - `Clutch.Size`: Average clutch size for each species.

  - `Eggs.per.Day`: Average number of eggs laid per day by each species.

  - `Incubation`: Average number of days spent in incubation (days
    between clutch complete and hatch).

  - `Nestling`: Average number of days spent in nestling period (days
    between hatch and fledge).

- output:

  character; An optional character string to custom name the output
  dataframe

## Value

dataframe

## Details

Precision of the fledge date estimates depend on what data is used to
estimate the values from (and if those values are estimated). Users
should consider if they want to estimate fledge dates based on estimated
hatch or lay dates. The function estimates fledge date by counting
forward from the hatch date (if available) the average number of
nestling days (provided by the user). If hatch date is not available,
the function estimates fledge date based on first lay date, countersign
forward the number of egg-laying-days based on clutch size (if
available, or user-provided average clutch size) and eggs-per-day of the
species.

## Examples

``` r
# Simplified NestWatch data with missing data
# All fledge dates if nest fledged (Outcome = s1) should be 2024-06-03
# Note Attemp.ID "4" failed to fledged (outcome = "f"), so a fledge date is not estimated
data <- data.frame(Attempt.ID = c("1", "2", "3", "4"),
                   Species.Code = rep("carwre", 4),
                   First.Lay.Date = as.Date(c("2024-05-01", "2024-05-02",
                                              "2042-05-01", "2024-05-01")),
                   Hatch.Date = as.Date(c("2024-05-21", NA, NA, "2024-05-21")),
                   Fledge.Date = as.Date(c(NA, NA, NA, NA)),
                   Fledge.Date.Estimated = c(NA, NA, NA, NA),
                   Clutch.Size = c(NA, 3, 4, NA),
                   Young.Total = c(4, 3, 4, 4),
                   Outcome = c("s1", "s1", "s1", "f"))

# Create phenology dataframe
phenology <- data.frame(Species = c("carwre"),
                        Clutch.Size  = c(4),
                        Eggs.per.Day = c(1),
                        Incubation   = c(16),
                        Nestling     = c(13))

nw.estfledge(data = data, phenology = phenology)
#> ... Estimating fledge dates, this may take some time ...
```
