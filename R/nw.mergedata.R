#' @title Merge NestWatch Datasets
#'
#' @param attempts dataframe; A dataframe containing NestWatch nest attempts data
#' @param checks dataframe; A dataframe containing NestWatch nest check data
#' @param output character; An optional character vector to custom name the output dataframe
#'
#' @return dataframe; A dataframe of merged nest attempts and checks unified on "Attemp.ID"
#' @export
#'
#' @examples
#' # Merge downloaded NestWatch datasets
#' nw.mergedata(NW.attempts, NW.checks)
#'
#' \dontrun{
#' # Specify the new dataframe name
#' nw.mergedata(NW.attempts, NW.checks, output = "merged_data")
#' }
nw.mergedata <- function(attempts, checks, output = NULL) {                                   # define mergedata function, with two required argument to select datasets, one optional to name output

    data <- left_join(x = attempts, y = checks, by = "Attempt.ID")                            # join attempts and checks together
    data <- data %>% rename(Observer.ID = Observer.ID.x) %>%                                  # rename observer ID column
                     select(-Observer.ID.y)                                                   # drop teh duplicate observer column (obs for attempts always = checks)
    data <- data %>% relocate(Species.Name, .after = Subnational.Code) %>%                    # reorder columns in a logical manor
                     relocate(Species.Code, .after = Species.Name) %>%
                     relocate(Year, .after = Species.Code) %>%
                     relocate(Elevation.m, .after = Year) %>%
                     relocate(Height.m, .after = Elevation.m) %>%
                     relocate(Substrate.Relationship, .after = Substrate) %>%
                     relocate(Predator.Guard, .after = Substrate.Other.Description) %>%
                     relocate(Predator.Guard.Other, .after = Predator.Guard)


    if (is.null(output)) {                                                                    # if output name is not provided
      merged.data <<- data                                                                    # name of new dataframe will be "data"
    } else {                                                                                  # if output name is provided
      assign(paste0(output), data, envir = .GlobalEnv)                                        # name of new dataframe will be argument of "output"

    }

}



