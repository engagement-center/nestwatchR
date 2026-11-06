# Conduct Common NestWatch Data Cleaning Procedures

This data cleaning vignette will cover several different methods for
cleaning data found within the [NestWatch Open
Dataset](https://data.mendeley.com/datasets/wjf794z7gc). NestWatch data
are collected by volunteer participants (researchers and the public) and
are known to contain some common errors and inconsistencies. Thus, these
data should be considered as “raw”, and the analyst should consider
their individual study system and research objectives when deciding what
cleaning procedures to conduct. This package contains two functions to
provide coarse-scale cleaning procedures that NestWatch staff have
identified in order to help analysts prepare NestWatch data for
analysis. Please read the full function documentation and this vignette
prior to cleaning the data.

\

## Background

NestWatch data entered in real-time from the U.S. and Canada are passed
through a series of filters that attempt to catch data-entry errors or
prevent unlikely responses in clutch size, number of young, timing of
nesting, incubation periods, and brooding periods. This real-time
feedback system does not identify all potential errors. For instance, if
a participant enters the number of eggs incorrectly but the typo in
clutch size is still biologically possible, no feedback will be given.
Even if real-time feedback is given, the participant may override the
warning if they feel the entry they have made is accurate. Participants
do occasionally observe unusually large clutch sizes or nests that have
occurred well outside of the traditional nesting period for the focal
species. It is up to each analyst to decide if these observations are
relevant. Importantly, these initial biological filters have not been
established for species nesting outside of the U.S. and Canada or for
species that are seldom reported.

NestWatch additionally implements a geographic review of species
location based on filters curated by [eBird](https://ebird.org/home).
For example, a participant should not be able to report a nest from a
species that is extremely rare at the locale, preventing many species
identification errors. Note that for similar sympatric species,
incorrect identifications are not flagged by this process (e.g., a
Black-capped Chickadee may be misidentified as a Carolina Chickadee in
regions of range overlap). However, older reports, including historic
data migrated to the NestWatch database, are not subjected to the same
review process. We recommend that data analysts map nest locations and
screen for likely location errors. See the
[`nw.filterspatial()`](https://engagement-center.github.io/nestwatchR/reference/nw.filterspatial.html)
and [Filtering
Vignette](https://engagement-center.github.io/nestwatchR/articles/d_Data-Filtering.html)
for more information on spatial filtering.

\

## Filtering & consolidating taxonomic units

The NestWatch platform uses [eBird
taxonomy](https://science.ebird.org/en/use-ebird-data/the-ebird-taxonomy)
to standardize species’ names. This allows participants to select any
available eBird taxonomic unit, including “spuhs” (i.e., “chickadee
sp.”), slashes (i.e., “Tree/Violet-green Swallow”), hybrids (i.e.,
“Mallard x American Black Duck (hybrid)”), and subspecies/forms (i.e.,
“Downy Woodpecker (eastern)”). The function
[`nw.cleantaxa()`](https://engagement-center.github.io/nestwatchR/reference/nw.cleantaxa.html)
helps filter out and/or “roll up” these taxonomic units. This function
uses a NestWatch dataframe as `data =` and has logical arguments
`rm_spuh =`, `rm_slash =`, `rm_hybrid =`, and `roll_subspecies =` where
`T` removes attempts with those spuh, slash, or hybrid designations and
subspecies/forms are “rolled up” to the species level (i.e., “Downy
Woodpecker (eastern)” → “Downy Woodpecker”). If arguments are set to `F`
or `NULL` the filtering and/or roll up are not performed. The resulting
dataframe may be optionally named with `output =`. This function changes
both the common names and the species codes to reflect the changes made
to the taxonomic unit.

Taxonomy updates to the participant interface and internal database are
implemented concurrently with updates from
[`auk::get_ebird_taxonomy()`](https://cornelllabofornithology.github.io/auk/reference/get_ebird_taxonomy.html).
However, new versions of the NestWatch Open Dataset are published in
January and may be taxonomically outdated for short periods of time
(this typically affects a limited number of records). Previous versions
of the NestWatch Open Dataset are not updated for taxonomic or other
corrections; we encourage the use of the most recent version. For more
information, see
[`auk::ebird_taxonomy`](https://cornelllabofornithology.github.io/auk/reference/ebird_taxonomy.html)
and [eBird
Taxonomy](https://science.ebird.org/en/use-ebird-data/the-ebird-taxonomy).

``` r

# Simple example dataframe
df <- data.frame(Attempt.ID = c(1, 1, 2, 3, 4),
                 Species.Name = c("Downy Woodpecker (Eastern)", "Downy Woodpecker (Eastern)",
                                 "chickadee sp.", "Tree/Violet-green Swallow",
                                 "Mallard x American Black Duck (hybrid)"),
                 Species.Code = c("dowwoo1", "dowwoo1", "chicka1", "y00701", "x00004"))

df
#>   Attempt.ID                           Species.Name Species.Code
#> 1          1             Downy Woodpecker (Eastern)      dowwoo1
#> 2          1             Downy Woodpecker (Eastern)      dowwoo1
#> 3          2                          chickadee sp.      chicka1
#> 4          3              Tree/Violet-green Swallow       y00701
#> 5          4 Mallard x American Black Duck (hybrid)       x00004
```

#### Spuhs & Slashes

Correct usage of a “spuh” or “slash” would, for example, be if a
participant observed a chickadee nesting in a region of Black-capped and
Carolina Chickadee range overlap and was unable to confirm the exact
species with certainty. The same would be true if a participant
monitored a nest box of a swallow species and never saw the adults at a
site where both Tree and Violet-green Swallows were present. In both
cases, either a slash or spuh could be appropriate.

When reasonable certainty of a specie(s) identity is needed, the analyst
may decide to omit spuhs and slashes:

``` r

# Remove spuhs and slashes
nw.cleantaxa(data = df, rm_spuh = T, rm_slash = T, output = "ex1")

ex1
#>   Attempt.ID                           Species.Name Species.Code
#> 1          1             Downy Woodpecker (Eastern)      dowwoo1
#> 2          1             Downy Woodpecker (Eastern)      dowwoo1
#> 3          4 Mallard x American Black Duck (hybrid)       x00004
```

#### Subspecies/forms & Hybrids

Some subspecies or forms are easily differentiated and may be reported
as such by a participant. Caution should be taken when using these
subspecies/form designations because the identification skills of
participants are unknown and the participant may not have selected the
correct designation from the pre-populated list of possible species.
Consider whether or not there may be biologically relevant differences
in nesting behavior, phenology, or other parameters between different
subspecies/forms before analyzing data at that level. If these potential
differences are not biologically relevant to the analysis, the analyst
may opt to `roll_subspecies = T` to combine subspecies and/or forms to
the species level.

Nesting attempts involving parents of different species, a hybrid
parent, or two hybrid parents, are very uncommon. The NestWatch database
has no way to easily differentiate between these three cases. Thus, if a
hybrid species code was entered for a nest attempt, the species identity
of the parents is uncertain. Additionally, participants may incorrectly
select a hybrid species when they should have selected a spuh or slash
species designation. If you are interested in validating attempts
labeled with hybrid status, you may [request additional
information](https://data.nestwatch.org/nw/contact) from NestWatch staff
asking for the “notes” fields which may contain additional information
not exported in the public dataset.

``` r

# Remove hybrids and roll up subspecies
nw.cleantaxa(data = df, rm_hybrid = T, roll_subspecies = T, output = "ex2")

ex2
#>   Attempt.ID              Species.Name Species.Code
#> 1          1          Downy Woodpecker       dowwoo
#> 2          1          Downy Woodpecker       dowwoo
#> 3          2             chickadee sp.      chicka1
#> 4          3 Tree/Violet-green Swallow       y00701
```

\

## Clean Data

The function
[`nw.cleandata()`](https://engagement-center.github.io/nestwatchR/reference/nw.cleandata.html)
is a collection of 11 common cleaning procedures which NestWatch staff
have identified to assist analysts in cleaning NestWatch data. You will
need to specify which procedures to run, lettered `a:k` (not case
sensitive), and we highly suggest you read the function documentation
and the following examples before selecting which procedures to run.
After these coarse scale cleaning procedures are completed, the user
should continue reviewing the data at a finer scale including
considering spatial and phenological filtering. See the [Filtering
Vignette](https://engagement-center.github.io/nestwatchR/articles/c_Data-Filtering.html)
and
[`nw.filterspatial()`](https://engagement-center.github.io/nestwatchR/reference/nw.filterspatial.html)
and
[`nw.filterphenology()`](https://engagement-center.github.io/nestwatchR/reference/nw.filterphenology.html)
functions.

The function
[`nw.cleandata()`](https://engagement-center.github.io/nestwatchR/reference/nw.cleandata.html)
takes a NestWatch dataframe as `data =` and allows you to specify the
function’s `mode =` as either `"flag"` or `"remove"`. When mode is set
to flag, nest attempts which do not meet the cleaning criteria will be
noted in a new column `Flagged.Attempt` with the value `FLAGGED` or
`NA`. If mode is set to remove, these flagged attempts are removed from
the output.

> ⚠️ If you are only interested in a subset of species, or RAM usage and
> processing time are a concern, you may choose to subset the data to
> your focal species prior to running
> [`nw.cleandata()`](https://engagement-center.github.io/nestwatchR/reference/nw.cleandata.html).

``` r

# Example of subsetting to 2 focal species: Carolina and Bewick's Wrens
# Using output "merged.data" from `nw.mergedata()`
df <- merged.data %>% filter(Species.Code %in% c("carwre", "bewwre"))

# Clean data function parameters
nw.cleandata(data = df, mode = "flag", methods = c("a", "b", "c") , output = "cleaned.data")
```

Individual cleaning procedures are specified in a character vector (not
case-sensitive) of letters `a:k`. An optional `output =` argument is
included to optionally rename the resulting dataframe as well.

\

### Cleaning Procedure Definitions

1.  **Obligate Brood Parasites:** Flag/remove attempts with
    `Species.Name` entered as species know to be obligate brood
    parasites. Species like Brown-headed Cowbirds are obligate brood
    parasites and do not create their own nests. This is not the correct
    interpretation of the nest species. Users may choose to look at
    these data more closely if investigating brood parasitism. To view a
    list of identified obligate brood parasites, [click
    here](#parasite-table).\
    \
2.  **No Nesting Activity:** Flag/remove attempts with Outcome of “no
    breeding behavior observed” (u3), “inactive” (i), and “not
    monitored” (n). These data likely represent nests or nest boxes
    where eggs were not laid, or nests that were not monitored.\
    \
3.  **Invasive Species Management:** Flag/remove attempts with Outcome
    of “invasive species management” (f5). For these attempts, the
    participant chose to remove/alter eggs or nests of invasive species.
    If the user is interested in analyzing participants’ habits in
    invasive species management or using some of the data fields (e.g.,
    clutch size, first lay date), they might consider skipping this
    method with the understanding that the outcome code is interpreted
    as “failure due to human interference” and standard nest survival
    estimates would not be meaningful.\
    \
4.  **Failed but fledged?:** Flag/remove attempts if outcome is a
    failure code (f, f1, f2, f3, f6, f7) but recorded fledged host young
    \> 0. In this case, a participant may have either characterized the
    nest’s outcome incorrectly, recorded the presence of fledged host
    young incorrectly, or mischaracterized brood parasite young as host
    young. If an attempt produces any number of fledged host young, the
    attempt is considered successful.\
    \
5.  **Successful but didn’t fledge?:** Flag/remove attempt if Outcome is
    success (s1), but recorded fledged host young = 0. Inverse of d.\
    \
6.  **More young than eggs?:** Flag/remove attempt if \# hatched host
    young \> clutch size. This may indicate incorrectly entered data or
    long lengths of time between nest checks where summary data was not
    properly updated. You may choose to review these attempts by looking
    with caution at the nest visit data to validate hatched young and
    clutch sizes.\
    \
7.  **More fledged young than hatched young:** Flag/remove attempt if \#
    fledged host young \> \# hatched host young. Similar to f.\
    \
8.  **Unmapped Region Code:** Flag/remove attempts for which NestWatch
    failed to identify a region (Subnational.Code == “XX-”).
    Subnational.Code is automatically assigned based on coordinates
    supplied by the participant. Many attempts identified as XX- likely
    resulted from nests being located in water bodies and/or
    participants entering incorrect coordinates. Consider removing the
    “XX” attempts if the coordinates are implausible for the focal
    species and consider inclusion if analyzing coastal or water-nesting
    species (e.g., Osprey nesting on channel markers).\
    \
9.  **Decrease-then-increase Eggs/Young:** Flag/remove attempts where
    the number of eggs or young decrease and then subsequently
    increases. This may happen if a nest fails and a new attempt is
    started at the same location, which should be two individual nesting
    attempts. If the analyst is looking at host response to depredation
    events or egg dumping you may choose to retain these records.\
    \
10. **Impossibly long attempts:** Flag/remove attempts with implausible
    nesting periods. Incorrect years are sometimes entered between date
    summary fields by participants. This may produce impossibly long
    nesting periods. To account for nesting phenologies in all
    hemispheres, this procedure identifies attempts in which (1) Fledge
    Date - First Lay Date \> 365 days, (2) Hatch Date - First Lay Date
    \> 84 days, or (3) Fledge Date - Hatch Date \> 300 days. These dates
    represent the maximum nest phenological period for any bird species
    and are not realistic for the majority of the NestWatch dataset. We
    encourage users to determine reasonable phenologies for their
    species of interest and use
    [`nw.filterphenology()`](https://engagement-center.github.io/nestwatchR/reference/nw.filterphenology.html)
    to run a finer filter on nest phenology dates.\
    \
11. **Year errors in visits:** Flag/remove attempts where the \# days
    between the first and the last visit are \> 365 days. Additional
    check to identify nest attempts where year portion of dates between
    nest visits were likely incorrectly entered. A user may choose to
    review these attempts individually to identify typos.\
    \

Depending on what cleaning procedures you choose to run, the sample size
of nests will decrease to varying degrees. If your species has a small
sample size after using `mode = "remove"` you may consider rerunning
with `mode = "flag"` and inspecting which attempts were removed and if
each cleaning method was appropriate for your analysis. Particularly for
cleaning steps `d:g`, you may prefer to nullify questionable fields
without deleting the entire attempt (e.g.,if your analysis only requires
location, species, and first lay date, then you may not need to remove
attempts with questionable outcome codes).

``` r

# Load and merge version 2 of dataset
nw.getdata(version = 2)
nw.mergedata(attempts = NW.attempts, checks = NW.checks, output = "merged.data")
length(unique(merged.data$Attempt.ID))
# > 648569    # Attempts in raw dataset


# Remove attempts with known brood parasites (like Brown-headed Cowbird) as the nest species
nw.cleandata(data = merged.data, mode = "remove", methods = "a", output = "no_parasite_spp")
length(unique(no_parasite_spp$Attempt.ID))
# > 648537    # Attempts after cleaning

# Remove attempts using "j" and "k"
nw.cleandata(data = merged.data, mode = "remove", methods = c("j", "k"), output = "no_longnests")
length(unique(no_longnests$Attempt.ID))
# > 646743    # Attempts after cleaning
```

\
\
\
\

### Reference Table

**Obligate Brood Parasite List**
