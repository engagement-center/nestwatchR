# Perform Common Cleaning Procedures to NestWatch Data

NestWatch data are collected by volunteer participants (researchers &
the public) and are known to contain some errors. This function provides
several common procedures that NestWatch staff have identified to help
the user clean NestWatch data for analysis. **The user should consider
their research objectives/species life histories and read the full
documentation below and in the vignette.**

## Usage

``` r
nw.cleandata(data, mode, methods, output = NULL)
```

## Arguments

- data:

  dataframe; A dataframe containing NestWatch data.

- mode:

  `"flag"` or `"remove"`; A character string defining if the user wants
  the identified nesting attempts to be flagged with "FLAGGED" in a new
  column. Or removed from the dataset.

- methods:

  character vector; A vector containing any of the letters `a:k` (not
  case- or order-sensitive), identifying each cleaning procedure to be
  conducted on the data. See **Details** below and the vignette for
  details on each procedure.

- output:

  character; An optional character string to custom name the output
  dataframe.

## Value

a dataframe

## Details

NestWatch data in the database should be considered "raw" and the user
should consider their system and research objectives when deciding what
cleaning procedures to conduct. This function contains 11 cleaning
procedures `a:k`.\
\
**Cleaning Procedure Details**:

- `a`: Flag/remove attempts with attempts with \`Species.Name\` entered
  as a species known to be an obligate brood parasite. These species do
  not create their own nests, so this is not the correct interpretation
  of the nest species. Users may choose to look at these data more
  closely if investigating brood parasitism. See the full list or
  obligate brood parasite species [click
  here](https://https://engagement-center.github.io/nestwatchR/articles/b_Data-Cleaning.html#parasite-table).

- `b`: Flag/remove attempts with of "no breeding behavior observed"
  (`u3`), "inactive" (`i`), and "not monitored" (`n`). These data likely
  represent nests or nest boxes which never received eggs or were
  unmonitored. Users may choose to include these data if looking at
  habitat or location data without measures of phenology or success.

- `c`: Flag/remove attempts with `Outcome` of “invasive species
  management” (`f5`). For these attempts, the participant chose to
  remove/alter eggs or nests of invasive species. If the user is
  interested in analyzing participants’ habits in invasive species
  management or using some of the data fields (e.g., clutch size, first
  lay date), they might consider skipping this method with the
  understanding that the outcome code is interpreted as “failure due to
  human interference” and standard nest survival estimates would not be
  meaningful. Note: “Invasive species management” may also be
  incorrectly chosen by participants trying to indicate nest failure
  caused by an invasive species, in which case it would still be prudent
  to flag or remove such records.

- `d`: Flag/remove attempts if outcome is a failure code
  (`f, f1, f2, f3, f6, f7`) but recorded fledged host young \> 0. In
  this case, a participant may have either characterized the nest's
  outcome incorrectly, recorded the presence of fledged host young
  incorrectly, or mischaracterized brood parasite young as host.
  Discerning the true outcome may not be possible. If an attempt
  produces any number of fledged host young, the attempt is considered
  successful.

- `e`: Flag/remove attempt if `Outcome` is success (`s1`), but recorded
  fledged host young = 0. Inverse of `d`.

- `f`: Flag/remove attempt if \# hatched host young \> clutch size. This
  may indicate incorrectly entered data or long lengths of time between
  nest checks where summary data was not properly updated. An analyst
  may choose to review these attempts by looking with caution at the
  nest visit data to validate hatched young and clutch sizes.

- `g`: Flag/remove attempt if \# fledged host young \> \# hatched host
  young. Similar to `f`.

- `h`: Flag/remove attempts for which NestWatch failed to identify a
  region (`Subnational.Code == “XX-”`). Subnational.Code is
  automatically assigned based on coordinates supplied by the
  participant. Many attempts identified as XX- likely resulted from
  nests being located in water bodies and/or participants entering
  incorrect coordinates. Consider removing the “XX” attempts if the
  coordinates are implausible for the focal species and consider
  inclusion if analyzing coastal or water-nesting species (e.g., Osprey
  nesting on channel markers).

- `i`: Flag/remove attempts where the number of eggs or young decrease
  and then subsequently increase. This may happen if a nest fails and a
  new attempt is started at the same location, which should be two
  individual nesting attempts. If the analyst is looking at host
  response to depredation events or egg dumping you may choose to retain
  these records.

- `j`: Flag/remove attempts with impossible nesting periods. Incorrect
  years are sometimes entered between date summary fields by
  participants. This may produce impossibly long nesting periods. To
  account for nesting phenologies in all hemispheres, this procedure
  identifies attempts in which (1) Fledge Date - First Lay Date \> 365
  days, (2) Hatch Date - First Lay Date \> 84 days, or (3) Fledge Date -
  Hatch Date \> 300 days. These dates represent the maximum nest
  phenological period for any bird species and are not realistic for the
  majority of the NestWatch dataset. We encourage users to determine
  reasonable phenologies for their species of interest and use
  [`nw.filterphenology`](https://engagement-center.github.io/nestwatchR/reference/nw.filterphenology.md)
  to run a finer filter on nest phenology dates by species.

- `k`: Flag/remove attempts where the \# days between the first and the
  last visit are \> 365 days. Additional check to identify nest attempts
  where year portion of dates between nest visits were likely
  incorrectly entered. An analyst may choose to review these attempts
  individually to verify if a typo occurred.

## Examples

``` r
# Load example wren data
wrens <- nestwatchR::wren_quickstart

# Flag data not meeting procedures e, f, g, and h
nw.cleandata(data = wrens, mode = "flag",
             methods = c("e", "f", "g", "h"))
#> ... Beginning to identify nesting attempts that do not meet the criteria. This may take a minute ...
#> ... Identified nesting attempts have been noted with 'FLAGGED' in the new column 'Flagged.Attempt'.

# Remove data not meeting procedures j or k.
nw.cleandata(data = wrens, mode = "remove",
             methods = c("j", "k"))
#> ... Beginning to identify nesting attempts that do not meet the criteria. This may take a minute ...
#> ... Identified nesting attempts have been removed from the new dataset.
```
