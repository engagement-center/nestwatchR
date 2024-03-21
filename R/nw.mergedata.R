#' @title Merge NestWatch Datasets
#'
#' @param attempts dataframe; A dataframe containing NestWatch nest attempts data
#' @param checks dataframe; A dataframe containing NestWatch nest check data
#' @param output character; An optional character vector to custom name the output dataframe
#'
#' @return dataframe; A dataframe of merged nest attempts and checks unified on "Attemp.ID"
#' @export
#'
#' @import dplyr
#' @examples
#'  \dontrun{
#' # Create simplified example data
#' NW.attempts <- data.frame
#'
#' # Merge downloaded NestWatch datasets
#' nw.mergedata(NW.attempts, NW.checks)
#'
#'
#' # Specify the new dataframe name
#' nw.mergedata(NW.attempts, NW.checks, output = "merged_data")
#' }
nw.mergedata <- function(attempts, checks, output = NULL) {

  # Join attempts and checks together
  data <- left_join(x = attempts, y = checks, by = "Attempt.ID")

  # Remove ".x" or ".y" from column names
  col_names <- colnames(data)
  clean_col_names <- gsub("\\.x$|\\.y$", "", col_names)
  colnames(data) <- clean_col_names

  # Reorder columns in a logical manor
  data <- data %>% relocate(Species.Name, .after = Subnational.Code) %>%
    relocate(Species.Code, .after = Species.Name) %>%
    relocate(Year, .after = Species.Code) %>%
    relocate(Elevation.m, .after = Year) %>%
    relocate(Height.m, .after = Elevation.m) %>%
    relocate(Substrate.Relationship, .after = Substrate) %>%
    relocate(Predator.Guard, .after = Substrate.Other.Description) %>%
    relocate(Predator.Guard.Other, .after = Predator.Guard)

  # If output name is specified
  if (is.null(output)) {
    merged.data <<- data
  } else {
    assign(paste0(output), data, envir = .GlobalEnv)

  }

}




