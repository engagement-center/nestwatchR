#' Filter Nesting Attempts Based on Expected Nest Phenological Periods
#'
#' @description To provide additional checks on data quality before analysis, a user may want to filter out nesting attempts which were
#' recorded to have nesting phenologies of unexpected lengths. This function allows a user to specify specie-specific values for nest phenologies
#' using expected days in the lay, incubation, nestling, and total nesting periods. By defining these allowable time frames a user can flag or
#' remove "run-on" nest attempts or nests which do not match the specie's life history.
#'
#'
#' @param data dataframe; A dataframe of merged NestWatch attempt and visits data. Date columns must have class \code{Date}
#' and Datetime columns must have class \code{POSIXct}.
#' @param sp character; A single character vector containing the species code for which to filter data for.
#' @param mode \code{"flag"} or \code{"remove"}; A character string defining if the user wants the identified nesting attempts to be
#' flagged with "FLAGGED" in a new column. Or removed from the dataset.
#' @param phenology numeric vector; A numeric vector of length 4 representing each of the following, in order:
#'  \itemize{
#'   \item \strong{Lay}: The number of days representing the maximum expected laying period.
#'   \item \strong{Incubation}: The number of days representing the maximum expected incubation period (between clutch complete and hatch).
#'   \item \strong{Nestling}: The number of days representing the maximum expected nesting period (hatch and last fledge).
#'   \item \strong{Total Nesting Period}: The number of days representing the maximum expected nesting period (spanning first lay to last fledge).
#'   }
#' @param output character; An optional character string to custom name the output dataframe
#'
#' @importFrom stringr str_detect
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' x <- 1+2
#' }
nw.filterphenology <- function(data, sp, mode, phenology, output = NULL){

  #####################################
  ###   Function Parameters Check   ###
  #####################################

  # stop if dataframe is not provided
  if(missing(data)){
    stop("Augument 'data' must be a dataframe of merged NestWatch attempts and visits data.")
  }
  # Stop if dataframe is not a NW merged dataframe
  if (all(!(c("Species.Code", "Visit.ID") %in% names(data)))){
    stop("Augument 'data' must be a dataframe of merged NestWatch attempts and visits data.")
  }
  # Stop if species is missing
  if (missing(sp)){
    stop("Argument 'sp' must only contain the species code of a single species.")
  }
  # Stops function if sp is more than one species
  if (length(sp) != 1){
    stop("Argument 'sp' must only contain the species code of a single species.")
  }
  # Stops function if sp is not a species code contained in Species.Code
  if(!sp %in% data$Species.Code){
    stop("Argument 'sp' must only contain a single species code found within the 'Species.Code' column.")
  }
  # Stops Function if 'mode' is not specified.
  if (missing(mode)) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  # Stops Function if 'mode' is invalid.
  if (!mode %in% c("flag", "remove")) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  # Stops function if phenology is not a character vector of length 5.
  if (!is.numeric(phenology) || length(phenology) != 4) {
    stop("Argument 'phenology' must be a 4 element vector (numeric or NA). See ?nw.filterphenology() for details.")
  }
  # Stops function if date is not incorrect class.
  if (!all(sapply(data[, c("First.Lay.Date", "Fledge.Date", "Hatch.Date")], inherits, "Date"))) {
    stop("Columns 'First.Lay.Date', 'Fledge.Date', 'Hatch.Date' must be of class 'Date'.")
  }
  # Stops function if date-time is incorrectly classed.
  if (!all(sapply(data$Visit.Datetime, function(x) inherits(x, c("POSIXct", "POSIXt"))))) {
    stop("Column 'Visit.Datetime' must be of class 'POSIXct' or 'POSIXt'.")
  }



  #####################################
  ###            Setup              ###
  #####################################

  # Initiate column names
  Flagged.Attempt <- Attempt.ID <- Species.Code <- First.Lay.Date <- Hatch.Date <- Fledge.Date <- NA
  Visit.Datetime <- max_date <- min_date <- date_difference <- NA


  # Prep dataframe
  data <- data %>% mutate(Flagged.Attempt = NA) %>%                             # Make new column to hold flag code
    relocate(Flagged.Attempt, .before = Attempt.ID)              # Reorder column to beginning
  spp_data <- data %>% filter(Species.Code == sp)                               # Filter the dataset to just the sp of interest

  # Define phenology vector elements
  lay <- phenology[1]
  inc <- phenology[2]
  nestling <- phenology[3]
  total <- phenology[4]


  #########################################
  ###        Flag Long Attempts         ###
  #########################################

  # Find attempts where Hatch - Lay > user input
  H_L <- lay + inc
  temp <- spp_data %>% filter(str_detect(First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
    filter(str_detect(Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
  temp <- temp %>% filter(as.integer(Hatch.Date - First.Lay.Date) > H_L)
  toflag <- c(toflag, unique(temp$Attempt.ID))
  toflag <- unique(toflag)

  # Find attempts where Fledge - Hatch > user input
  H_F <- nestling
  temp <- spp_data %>% filter(str_detect(Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
    filter(str_detect(Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
  temp <- temp %>% filter(as.integer(Fledge.Date - Hatch.Date) > H_F)
  toflag <- c(toflag, unique(temp$Attempt.ID))
  toflag <- unique(toflag)

  # Find attempts where Fledge - Lay > user input
  L_F <- lay + nestling
  temp <- spp_data %>% filter(str_detect(First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
    filter(str_detect(Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
  temp <- temp %>% filter(as.integer(Fledge.Date - First.Lay.Date) > L_F)
  toflag <- unique(temp$Attempt.ID)

  # Find long Total nesting periods (from checks to account for run-on nests or no summary data provided)
  temp <- spp_data %>% filter(!is.na(Visit.Datetime))
  temp <- temp %>% group_by(Attempt.ID) %>%
    summarize(min_date = min(Visit.Datetime),
              max_date = max(Visit.Datetime),
              date_difference = as.integer(difftime(max_date, min_date, units = "days")))
  toflag <- temp %>% filter(date_difference > total) %>% pull(Attempt.ID) %>% unique()



  # Flag in original dataset and Prep for export
  pos <- 1
  envir = as.environment(pos)

  rows <- which(data$Attempt.ID %in% toflag)
  data$Flagged.Attempt[rows] <- "FLAGGED"


  #####################################
  ###        Mode == "flag"         ###
  #####################################

  # If mode == "flag"
  if (mode == "flag") {
    # Export resulting dataframe
    if (is.null(output)) {
      filtered.data <- NULL
      assign("filtered.data", data, envir = envir)
    } else {
      assign(paste0(output), data, envir = envir)
    }
    message("... Identified nesting attempts have been noted with 'FLAGGED' in the new dataset in column 'Flagged.Attempt'.")
  } else {


    #####################################
    ###       Mode == "remove"        ###
    #####################################

    # if mode was remove
    # Filter out any nest attempts that were flagged and remove column used to flag
    data <- data[!grepl("FLAGGED", data$Flagged.Attempt), ]
    data <- data %>% select(-Flagged.Attempt)

    # Export resulting dataframe
    if (is.null(output)) {
      cleaned.data <- NULL
      assign("filtered.data", data, envir = envir)
    } else {
      assign(paste0(output), data, envir = envir)
    }
    message("... Identified nesting attempts have been removed from the new dataset.")
  }













} # end function
