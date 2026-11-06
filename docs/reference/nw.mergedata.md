# Merge NestWatch Datasets

Merge NestWatch Datasets

## Usage

``` r
nw.mergedata(attempts, checks, output = NULL)
```

## Arguments

- attempts:

  dataframe; A dataframe containing NestWatch nest attempts data

- checks:

  dataframe; A dataframe containing NestWatch nest check data

- output:

  character; An optional character vector to custom name the output
  dataframe

## Value

dataframe; A dataframe of merged nest attempts and checks unified on
"Attempt.ID"

## Examples

``` r

# Load example NestWatch data
ex.attempts <- nestwatchR::exNWattempts
ex.checks <- nestwatchR::exNWchecks

# Merge example NestWatch datasets
nw.mergedata(ex.attempts, ex.checks)

if (FALSE) { # \dontrun{
# Optionally specify the new dataframe name
nw.mergedata(ex.attempts, ex.checks, output = "merged_data")
} # }
```
