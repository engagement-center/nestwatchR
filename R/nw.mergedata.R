#' @title Merge NestWatch Datasets
#'
#' @param attempts dataframe; A dataframe containing NestWatch nest attempts data
#' @param checks dataframe; A dataframe containing NestWatch nest check data
#' @param output character; An optional character vector to custom name the output dataframe
#'
#' @return dataframe; A dataframe of merged nest attempts and checks unified on "Attempt.ID"
#' @export
#'
#' @import dplyr
#' @importFrom lubridate parse_date_time
#'
#' @examples
#'
#' # Load example NestWatch data
#' ex.attempts <- nestwatchR::exNWattempts
#' ex.checks <- nestwatchR::exNWchecks
#'
#' # Merge example NestWatch datasets
#' nw.mergedata(ex.attempts, ex.checks)
#'
#' \dontrun{
#' # Optionally specify the new dataframe name
#' nw.mergedata(ex.attempts, ex.checks, output = "merged_data")
#' }
nw.mergedata <- function(attempts, checks, output = NULL) {

  # Initiate column names for call later
  Attempt.ID <- Species.Name <- Subnational.Code <- Year <- Species.Code <- Elevation.m <- Height.m <- NULL
  Substrate.Relationship <- Substrate <- Predator.Guard <- Substrate.Other.Description <- Predator.Guard.Other <- NULL


  # Join datatsets
  data <- left_join(x = attempts, y = checks, by = "Attempt.ID")

  # Remove ".x" or ".y" from column names, remove repeated data columns
  data <- data[, !names(data) %in% c("...1.x", "...1.y", "Observer.ID.y")]
  col_names <- colnames(data)
  clean_col_names <- gsub("\\.x$|\\.y$", "", col_names)
  colnames(data) <- clean_col_names

  # Reorder columns in a logical manner
  data <- data %>% relocate(Species.Name, .after = Subnational.Code) %>%
    relocate(Species.Code, .after = Species.Name) %>%
    relocate(Year, .after = Species.Code) %>%
    relocate(Elevation.m, .after = Year) %>%
    relocate(Height.m, .after = Elevation.m) %>%
    relocate(Substrate.Relationship, .after = Substrate) %>%
    relocate(Predator.Guard, .after = Substrate.Other.Description) %>%
    relocate(Predator.Guard.Other, .after = Predator.Guard)

  # Make dates class date
  data$First.Lay.Date <- as.Date(data$First.Lay.Date)
  data$Fledge.Date <- as.Date(data$Fledge.Date)
  data$Hatch.Date <- as.Date(data$Hatch.Date)
  data$Visit.Datetime <- parse_date_time(data$Visit.Datetime, orders = c("Y-m-d H:M:S", "Y-m-d")) # datetime handling fix

  # Prep for and output
  pos <- 1
  envir = as.environment(pos)
  if (is.null(output)) {
    assign("merged", data, envir = envir)
  } else {
    assign(paste0(output), data, envir = envir)

  }

}

