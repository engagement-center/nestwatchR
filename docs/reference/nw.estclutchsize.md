# Estimate Missing Summary Clutch Size Values in NestWatch Data

Clutch size is an important value for estimating nest dates and
determining the successfulness of a nest. NestWatch participants may
forget to enter clutch size into the nest attempt's summary data. But
clutch size information may be estimated from the data of individual
nest visits. This function used available data from any entered nest
visits to estimate maximum host egg count (clutch size).

## Usage

``` r
nw.estclutchsize(data, output = NULL)
```

## Arguments

- data:

  dataframe; A dataframe containing merged NestWatch attempts and visits
  data.

- output:

  character; An optional character vector to rename the resulting
  output.

## Value

dataframe; A dataframe with an additional binary column
`Clutch.Size.Estimated` where `1` denotes the clutch size was estimated
by the function and `0` denotes the clutch size was provided by the
participant.

## Details

This function looks for data contained in several columns to estimate
what the clutch size of the nest was. Because NestWatch participants do
not necessarily visit each nest everyday or record all data fields on
every visit the data needed to estimate clutch size may be spread over
many columns. Depending on the data available, the certainty of the
clutch size estimate may vary, or in some cases, may not be estimable.
For instance, a successful nest with 4 eggs recorded on several checks
and 4 fledged young observed would have high certainty that the clutch
size was 4. However, if a nest was observed with 2 eggs on one check and
a week later was found predated, the certainty of the clutch size is not
known. In this case the clutch size would be estimated as 2 but may have
been higher depending on the exact (unobserved) failure date.

## Examples

``` r
# Example simplified NestWatch dataset:
data <- data.frame(Attempt.ID = c(1, 2, 3, 4, 5, 6),
                   Visit.ID = c(1, 2, 3, 4, 5, 6),
                   Clutch.Size = c(1, NA, NA, NA, NA, NA),
                   Host.Eggs.Count = c(1, 2,  NA, NA, NA, 3),
                   Young.Total = c(1, 2, 3, 3, 4, NA),
                   Live.Host.Young.Count = c(1, 1, 2, 3, 1, 1),
                   Unhatched.Eggs = c(0, 0, NA, NA, 1, 3),
                   Young.Fledged = c(1, 0, NA, NA, 1, 3),
                   Dead.Host.Young.Count = c(0, NA, NA, 1, 0,1))

# Estimate Clutch Sizes if NA
out <- nw.estclutchsize(data = data)

out$Clutch.Size   # clutch size values
#> [1] 1 2 3 4 5 6
out$Clutch.Size.Estimated   # binary indicator if clutch size is estimated (1) or not (0)
#> [1] 0 1 1 1 1 1
```
