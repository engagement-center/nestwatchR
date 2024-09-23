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
#' @param sp character vector; A character vector containing the species code for one or more species for which to filter the data.
#' @param mode \code{"flag"} or \code{"remove"}; A character string defining if the user wants the identified nesting attempts to be
#' flagged with "FLAGGED" in a new column. Or removed from the dataset.
#' @param phenology dataframe; A simple dataframe with one row for each species of interest and the following column structure:
#'  \itemize{
#'   \item \strong{Species}: Species for which this row of data represents. Must be a 6-letter species code.
#'   \item \strong{Lay}: The number of days representing the maximum expected laying period for each species.
#'   \item \strong{Incubation}: The number of days representing the maximum expected incubation period (between clutch complete and hatch) for each species.
#'   \item \strong{Nestling}: The number of days representing the maximum expected nesting period (hatch and last fledge) for each species.
#'   \item \strong{Total Nesting Period}: The number of days representing the maximum expected nesting period (spanning first lay to last fledge) for each species.
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
    stop("Argument 'sp' must contain a vector of at least one 6-letter species code found within the 'Species.Code' column.")
  }
  # Stops function if sp is not a species code contained in Species.Code
  #if(!sp %in% data$Species.Code){
  #  stop("Argument 'sp' must contain a vector of at least one 6-letter species code found within the 'Species.Code' column.")
  #}
  # Stops Function if 'mode' is not specified.
  if (missing(mode)) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  # Stops Function if 'mode' is invalid.
  if (!mode %in% c("flag", "remove")) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  # Stops function if phenology is not a character vector of length 5.
  if (!is.data.frame(phenology) || ncol(phenology) != 5) {
    stop("Argument 'phenology' must be a dataframe with 5 columns. See ?nw.filterphenology() for details.")
  }
  # Stops function if phenology dataframe does not have the correct column names.
  required_cols <- c("species", "lay", "incubation", "nestling", "total")
  missing_cols <- required_cols[!sapply(required_cols, function(col) {  # Check if all required columns are present, ignoring case
                    any(grepl(col, colnames(phenology), ignore.case = TRUE))
  })]
  if (length(missing_cols) > 0) {
    stop(paste("The following required columns are missing:", paste(missing_cols, collapse = ", ")))
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

  phen <- phenology                                                             # shorter name
  spp_col <- grep("species", colnames(phen), ignore.case = TRUE)                # get the spp column index

  for (sp in phen[[spp_col]]) {for (sp in phen[[spp_col]]) {

    spp_data <- data %>% filter(Species.Code == sp)                         # Filter the dataset to just the sp of interest
    spp_col <- grep("species", colnames(phen), ignore.case = TRUE)          # get the spp column index
    spp_phen <- phen[phen[[spp_col]] == sp, ]                               # filter phenology data to just the spp of interest

    # Define phenology vector elements by searching for column names
    lay <- spp_phen[[grep("lay", colnames(spp_phen), ignore.case = TRUE)]]
    inc <- spp_phen[[grep("incubation", colnames(spp_phen), ignore.case = TRUE)]]
    nestling <- spp_phen[[grep("nestling", colnames(spp_phen), ignore.case = TRUE)]]
    total <- spp_phen[[grep("total", colnames(spp_phen), ignore.case = TRUE)]]


    #########################################
    ###        Flag Long Attempts         ###
    #########################################

    # Find attempts where Hatch - Lay > user input
    H_L <- lay + inc
    temp <- spp_data %>% filter(str_detect(First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
      filter(str_detect(Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
    temp <- temp %>% filter(as.integer(Hatch.Date - First.Lay.Date) > H_L)
    toflag <- c(unique(temp$Attempt.ID))


    # Find attempts where Fledge - Hatch > user input
    H_F <- nestling
    temp <- spp_data %>% filter(str_detect(Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
      filter(str_detect(Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
    temp <- temp %>% filter(as.integer(Fledge.Date - Hatch.Date) > H_F)
    flag <- unique(temp$Attempt.ID)
    toflag <- unique(c(toflag, flag))

    ### # Find attempts where Fledge - Lay > user input
    L_F <- lay + inc + nestling
    temp <- spp_data %>% filter(str_detect(First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
      filter(str_detect(Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
    temp <- temp %>% filter(as.integer(Fledge.Date - First.Lay.Date) > L_F)
    flag <- unique(temp$Attempt.ID)
    toflag <- unique(c(toflag, flag))

    # Find long Total nesting periods (from checks to account for run-on nests or no summary data provided)
    temp <- spp_data %>% filter(!is.na(Visit.Datetime))
    temp <- temp %>% group_by(Attempt.ID) %>%
      summarize(min_date = min(Visit.Datetime),
                max_date = nth(sort(Visit.Datetime, decreasing = TRUE), 2),
                date_difference = as.integer(difftime(max_date, min_date, units = "days")))
    flag <- temp %>% filter(date_difference > total) %>% pull(Attempt.ID) %>% unique()
    toflag <- unique(c(toflag, flag))



    # Flag in original dataset
    rows <- which(data$Attempt.ID %in% toflag)
    data$Flagged.Attempt[rows] <- "FLAGGED"


  } # end loop over each species

    spp_data <- data %>% filter(Species.Code == sp)                         # Filter the dataset to just the sp of interest
    spp_col <- grep("species", colnames(phen), ignore.case = TRUE)          # get the spp column index
    spp_phen <- phen[phen[[spp_col]] == sp, ]                               # filter phenology data to just the spp of interest

    # Define phenology vector elements by searching for column names
    lay <- spp_phen[[grep("lay", colnames(spp_phen), ignore.case = TRUE)]]
    inc <- spp_phen[[grep("incubation", colnames(spp_phen), ignore.case = TRUE)]]
    nestling <- spp_phen[[grep("nestling", colnames(spp_phen), ignore.case = TRUE)]]
    total <- spp_phen[[grep("total", colnames(spp_phen), ignore.case = TRUE)]]


    #########################################
    ###        Flag Long Attempts         ###
    #########################################

    # Find attempts where Hatch - Lay > user input
    H_L <- lay + inc
    temp <- spp_data %>% filter(str_detect(First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
      filter(str_detect(Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
    temp <- temp %>% filter(as.integer(Hatch.Date - First.Lay.Date) > H_L)
    toflag <- c(unique(temp$Attempt.ID))


    # Find attempts where Fledge - Hatch > user input
    H_F <- nestling
    temp <- spp_data %>% filter(str_detect(Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
      filter(str_detect(Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
    temp <- temp %>% filter(as.integer(Fledge.Date - Hatch.Date) > H_F)
    flag <- unique(temp$Attempt.ID)
    toflag <- unique(c(toflag, flag))

    ### # Find attempts where Fledge - Lay > user input
    L_F <- lay + inc + nestling
    temp <- spp_data %>% filter(str_detect(First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
      filter(str_detect(Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
    temp <- temp %>% filter(as.integer(Fledge.Date - First.Lay.Date) > L_F)
    flag <- unique(temp$Attempt.ID)
    toflag <- unique(c(toflag, flag))

    # Find long Total nesting periods (from checks to account for run-on nests or no summary data provided)
    temp <- spp_data %>% filter(!is.na(Visit.Datetime))
    temp <- temp %>% group_by(Attempt.ID) %>%
      summarize(min_date = min(Visit.Datetime),
                max_date = nth(sort(Visit.Datetime, decreasing = TRUE), 2),
                date_difference = as.integer(difftime(max_date, min_date, units = "days")))
    flag <- temp %>% filter(date_difference > total) %>% pull(Attempt.ID) %>% unique()
    toflag <- unique(c(toflag, flag))



    # Flag in original dataset
    rows <- which(data$Attempt.ID %in% toflag)
    data$Flagged.Attempt[rows] <- "FLAGGED"


  } # end loop over each species


  #####################################
  ###        Mode == "flag"         ###
  #####################################

  pos <- 1
  envir <- as.environment(pos)

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
