# Filter Nest Locations by Expected Range Maps

Use a geospatial polygon to identify or remove points located outside
the polygon. This function also includes an option buffer argument which
can be used to add a specific distance buffer around the polygon prior
to filtering.

## Usage

``` r
nw.filterspatial(
  points,
  polygon,
  mode,
  buffer = F,
  buffer_units = NULL,
  buffer_output = NULL,
  proj = NULL,
  output = NULL
)
```

## Arguments

- points:

  sf; A spatial feature containing point data.

- polygon:

  sf; A spatial polygon representing the area for which you want to
  include datafrom.

- mode:

  `"flag"` or `"remove"`; A character string defining if the user wants
  the identified nesting attempts to be flagged with "FLAGGED" in a new
  column. Or removed from the dataset.

- buffer:

  numeric; Optional distance to buffer around the polygon

- buffer_units:

  `"km"` or `"mi"`; A character string defining the buffer distance
  units as either kilometers or miles.

- buffer_output:

  logical; Optionally export the generated buffered polygon to the
  global enviornment.

- proj:

  a PROJ.4 string; Optional definition for map projection. Defaults to
  Lambert Conformal Conic.

- output:

  character; An optional character string to custom name the output
  spatial dataframe.

## Value

sf; A spatial dataframe

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
set.seed(123)

# Define simple polygon
square_coords <- matrix(c(0, 0,   # bottom-left
                          1, 0,   # bottom-right
                          1, 1,   # top-right
                          0, 1,   # top-left
                          0, 0),  # close the square back to bottom-left
                        ncol = 2, byrow = TRUE)

# Create an `sf` polygon
square_polygon <- sf::st_polygon(list(square_coords)) %>%
                  sf::st_sfc() %>% sf::st_sf() %>%
                  sf::st_set_crs(4326)  # Set a coordinate reference system (WGS84)

# Generate random points around the square, convert to sf object
random_points <- data.frame(Attempt.ID = seq(1:30),
                            x = runif(30, -0.5, 1.5),
                            y = runif(30, -0.5, 1.5))
points_sf <- sf::st_as_sf(random_points, coords = c("x", "y"), crs = 4326)

# Run the spatial filter
nw.filterspatial(points = points_sf,
                  polygon = square_polygon,
                  mode = "flag",
                  proj = "+proj=longlat +datum=WGS84 +no_defs",
                  output = "geofiltered.data")
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> ... Identified nesting attempts have been noted with 'FLAGGED' in the new dataset in column 'Flagged.Location'.
```
