#' Filter Nest Locations by Expected Range Maps
#'
#' @description Use a geospatial polygon to identify or remove points located outside the polygon. This function also includes an
#' option buffer argument which can be used to add a specific distance buffer around the polygon prior to filtering.
#'
#' @param points sf; A spatial feature containing point data.
#' @param polygon sf; A spatial polygon representing the area for which you want to include datafrom.
#' @param mode \code{"flag"} or \code{"remove"}; A character string defining if the user wants the identified nesting attempts to be flagged with "FLAGGED" in a new column. Or removed from the dataset.
#' @param buffer numeric; Optional distance to buffer around the polygon
#' @param buffer_units \code{"km"} or \code{"mi"}; A character string defining the buffer distance units as either kilometers or miles.
#' @param buffer_output logical; Optionally export the generated buffered polygon to the global enviornment.
#' @param proj a PROJ.4 sting; Optional definition for map projection. Defaults to Lambert Conformal Conic.
#' @param output character; An optional character string to custom name the output spatial dataframe.
#'
#' @importFrom sf st_transform
#' @importFrom sf st_buffer
#' @importFrom sf st_intersection
#'
#' @return sf; A spatial dataframe
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' # Define simple polygon
#' square_coords <- matrix(c(0, 0,   # bottom-left
#'                           1, 0,   # bottom-right
#'                           1, 1,   # top-right
#'                           0, 1,   # top-left
#'                           0, 0),  # close the square back to bottom-left
#'                         ncol = 2, byrow = TRUE)
#'
#' # Create an `sf` polygon
#' square_polygon <- sf::st_polygon(list(square_coords)) %>%
#'                   sf::st_sfc() %>% sf::st_sf() %>%
#'                   sf::st_set_crs(4326)  # Set a coordinate reference system (WGS84)
#'
#' # Generate random points around the square, convert to sf object
#' random_points <- data.frame(Attempt.ID = seq(1:30),
#'                             x = runif(30, -0.5, 1.5),
#'                             y = runif(30, -0.5, 1.5))
#' points_sf <- sf::st_as_sf(random_points, coords = c("x", "y"), crs = 4326)
#'
#' # Run the spatial filter
#' nw.filterspatial(points = points_sf,
#'                   polygon = square_polygon,
#'                   mode = "flag",
#'                   proj = "+proj=longlat +datum=WGS84 +no_defs",
#'                   output = "geofiltered.data")
#'

nw.filterspatial  <- function(points, polygon, mode, buffer = F, buffer_units = NULL, buffer_output = NULL, proj = NULL, output = NULL) {

  #####################################
  ###   Function Parameters Check   ###
  #####################################

  # Stop function if points is not an sf object
  if (!inherits(points, "sf")) {
    stop("Error: 'points' object must be of class 'sf'. Use sf::st_as_sf to convert dataframe into an sf object.")
  }
  # Stop function if polygons is not an sf object
  if (!inherits(polygon, "sf")) {
    stop("Error: 'polygon' object must be a shapefile of class 'sf'. See sf::st_as_sf")
  }
  # Stop function if mode is missing
  if (missing(mode)) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  # Stops function if 'mode' is invalid
  if (!mode %in% c("flag", "remove")) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  # Stop function if 'buffer' is provided and is not numeric
  if (isTRUE(buffer) && !is.numeric(buffer)) {
    stop("Invalid 'buffer'. Buffer must be a numeric value.")
  }
  # Stop function if 'buffer_units' is provided and is not either "km" or "mi"
  if (!is.null(buffer_units)) {
    if(!buffer_units %in% c("km", "mi")){
      stop("Invalid 'buffer_units'. Please provide either 'km' for kilometers or 'mi' for miles.")
    }
  }
  # Stop function if one of 'buffer' and 'buffer_units' is provided but the other is not
  if (isTRUE(buffer)){
    if (is.null(buffer_units)) {
      stop("Missing 'buffer_units'. Please provide the units 'km' or 'mi' for the buffer.")
    }
  }
  if (!is.null(buffer_units)){
    if (is.null(buffer)) {
      stop("Missing 'buffer'. Please provide the distance to be bufferd.")
    }
  }
  # Stops function if proj is length > 1
  if (!missing(proj)) {
    if(length(proj) > 1) {
      stop("Error: 'proj' argument must be a PROJ.4 string. This argument is optional and defaults to the Lambert Confomal Conic projection. If you would like to reproject the data to a different projection, search https://epsg.io/ or other site for your projection and copy the PROJ.4 string. Note: NestWatch and eBirdST data use the WGS84 datum.")
    }
  }
  # Stops function if 'buffer_output' is not logical
  if(isTRUE(buffer) & !is.null(buffer_output)){
    if(!is.logical(buffer_output)) {
    stop("Invalid 'buffer_output'. Value must be logical T or F.")
    }
  }
  # Stops function if proj is not a PROJ.4 string
  if (!missing(proj)) {
    # If proj is a character string
    if (is.character(proj)) {
      # Stops function if proj is not a PROJ.4 string
      if (!grepl("^\\+proj", proj)) {
        stop("Error: 'proj' argument must be a PROJ.4 string. This argument is optional and defaults to the Lambert Confomal Conic projection. If you would like to reproject the data to a different projection, search https://epsg.io/ or other site for your projection and copy the PROJ.4 string. Note: NestWatch and eBirdST data use the WGS84 datum.")
      }
    } else {
      # Stop if proj is not a character string
      stop("Error: 'proj' argument must be a PROJ.4 string. This argument is optional and defaults to the Lambert Confomal Conic projection. If you would like to reproject the data to a different projection, search https://epsg.io/ or other site for your projection and copy the PROJ.4 string. Note: NestWatch and eBirdST data use the WGS84 datum.")
    }
  }


  #####################################
  ###   DF Setup                    ###
  #####################################

  # Initiate NestWatch column names
  Attempt.ID <- Flagged.Location <- NULL

  # Set up existing points dataframe for flagging
  points <- points %>% mutate(Flagged.Location = NA)


  #####################################
  ###   Project Datasets            ###
  #####################################

  # Define projection
  # If custom projection is provided use it, if not use LCC
  if (!missing(proj)) {
    projection <- proj
  } else {
    projection <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
  }

  # Project points data
  points <- st_transform(points, crs = projection)

  # Project range polygon
  polygon <- st_transform(polygon, crs = projection)


  #####################################
  ###       If Buffering            ###
  #####################################

  # If a buffer distance was provided
  if (!missing(buffer)) {
    if (buffer_units == "km") {
      polygon <- st_buffer(polygon, dist = buffer * 1000)                       # create a km buffer
    } else {
      polygon <- st_buffer(polygon, dist = buffer * 1.069 * 1000)               # or create a mi buffer
    }
    inside <- st_intersection(points, polygon) %>% pull(Attempt.ID) %>% unique() # get the Attempt.ID of points inside the buffered polygon
    rows <- which(points$Attempt.ID %in% inside)                                # get row numbers of Attempts to keep
    points$Flagged.Location[-rows] <- "FLAGGED"                                 # flag attempts that were outside the buffered polygon

    # If outputting buffered polygon
    # Prep for output, then output
    pos <- 1
    envir = as.environment(pos)
    if (buffer_output) {
      assign("polygon_buffered", polygon, envir = envir)
    }
  } # end buffer loop


  #####################################
  ###       If not Buffering        ###
  #####################################

  if (missing(buffer)) {
    inside <- st_intersection(points, polygon) %>% pull(Attempt.ID) %>% unique() # get the Attempt.ID of points inside the buffered polygon
    rows <- which(points$Attempt.ID %in% inside)                            # get row numbers of Attempts to keep
    points$Flagged.Location[-rows] <- "FLAGGED"                             # flag attempts that were outside the buffered polygon
  }


  #####################################
  ###        Mode == "flag"         ###
  #####################################

  if (mode == "flag") {
    # Export resulting dataframe
    # Prep for output, then output
    pos <- 1
    envir = as.environment(pos)

    if (is.null(output)) {                                                      # if output name is not provided
      assign("geofiltered.data", points, envir = envir)                         # name of new dataframe will be "geofiltered.data"
    } else {                                                                    # if output name is provided
      assign(paste0(output), points, envir = envir)                        # name of new dataframe will be argument of "output"
    }
    message("... Identified nesting attempts have been noted with 'FLAGGED' in the new dataset in column 'Flagged.Location'.")
  } else {

    #####################################
    ###       Mode == "remove"        ###
    #####################################

    # if mode was remove
    # Filter out any nest attempts that were flagged and remove column used to flag
    points <- points[!grepl("FLAGGED", points$Flagged.Location), ]
    points <- points %>% select(-Flagged.Location)

    # Export resulting dataframe
    # Prep for output, then output
    pos <- 1
    envir = as.environment(pos)

    if (is.null(output)) {                                                        # if output name is not provided
      assign("geofiltered.data", points, envir = envir)                           # name of new dataframe will be "geofiltered.data"
    } else {                                                                      # if output name is provided
      assign(paste0(output), points, envir = envir)                          # name of new dataframe will be argument of "output"
    }
    message("... Identified nesting attempts have been removed from the new dataset.")

  } # end of mode = remove chunk




} # end

