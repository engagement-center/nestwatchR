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
#'
#' # load example NestWatch data
#' NW.attempts <- nestwatchR::exNWattempts
#' NW.checks <- nestwatchR::exNWchecks
#'
#' # Merge downloaded NestWatch datasets
#' nw.mergedata(NW.attempts, NW.checks)
#'
#' \dontrun{
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
  data <- data %>% relocate(.data$Species.Name, .after = .data$Subnational.Code) %>%
    relocate(.data$Species.Code, .after = .data$Species.Name) %>%
    relocate(.data$Year, .after = .data$Species.Code) %>%
    relocate(.data$Elevation.m, .after = .data$Year) %>%
    relocate(.data$Height.m, .after = .data$Elevation.m) %>%
    relocate(.data$Substrate.Relationship, .after = .data$Substrate) %>%
    relocate(.data$Predator.Guard, .after = .data$Substrate.Other.Description) %>%
    relocate(.data$Predator.Guard.Other, .after = .data$Predator.Guard)

  # Prep for and output
  pos <- 1
  envir = as.environment(pos)
  if (is.null(output)) {
    assign("merged.data", data, envir = envir)
  } else {
    assign(paste0(output), data, envir = envir)

  }

}




