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
#' @details This function calculates the number of days between \code{First.Lay.Date}, \code{Hatch.Date}, and \code{Fledge.Date} date values, comparing
#' each to the user-provided acceptable date spans for each nest phase. Not all attempts contain these summary dates. An analyst may choose to explore
#' \link[nestwatchR]{nw.estclutchsize}, \link[nestwatchR]{nw.estfirstlay}, \link[nestwatchR]{nw.esthatch}, and \link[nestwatchR]{nw.estfledge} functions
#' to estimate these summary dates from the individual nest visit data prior to using this function. As an additional check, this function calculates
#' the date span between the first and second to last nest check and compares this to the user-provided "total nesting period" value. The second to last
#' date is used because participants may not check a nest when close to fledgling. For example, a participant may attempt to confirm a successful
#' fledged nest when emptying a nets box at the end of the nesting season. In this case the last "nest check" may be weeks to months after the last
#' activity at that nest.
#'
#'
#'
#'
#' @importFrom stringr str_detect
#'
#' @return dataframe
#' @export
#'
#' @examples
#' # Create phenology dataframe
#' phenology <- data.frame(species = c("carwre"),
#'                         lay = c(7),          # max observed
#'                         incubation = c(20),  # mean plus some extra
#'                         nestling = c(20),    # mean plus some extra
#'                         total = c(50))       # mean plus some extra
#'
#'
#' # Simplified NestWatch dataset with nest summary dates
#' # Attempts 3 & 4 should be flagged (too long in incuation and nestling phases respectively)
#' data <- data.frame(Attempt.ID = c("1", "2", "3", "4"),
#'                    Species.Code = rep("carwre", 4),
#'                    First.Lay.Date = as.Date(c("2024-05-01", "2024-05-01",
#'                                               "2024-05-01", "2024-05-01")),
#'                    Hatch.Date = as.Date(c("2024-05-20", "2024-05-21", "2024-06-10", "2024-05-21")),
#'                    Fledge.Date = as.Date(c("2024-06-05", NA, "2024-06-25", "2024-06-30")),
#'                    Visit.Datetime = c(rep(NA, 4)),
#'                    Outcome = c("s1", "f", "s1", "s1"),
#'                    Nest.Status = rep(NA, 4))
#' nw.filterphenology(data = data, phenology = phenology, mode = "flag")
#'
#'
#' # Simplified NestWatch dataset without nest summary dates (will look at total nest attempt duration from visit dates)
#' # Attempt "2" should be flagged as being too long.
#' data <- data.frame(Attempt.ID = c("1", "1", "2", "2"),
#'                    Species.Code = rep("carwre", 4),
#'                    First.Lay.Date = as.Date(rep(NA, 4)),
#'                    Hatch.Date = as.Date(rep(NA, 4)),
#'                    Fledge.Date = as.Date(rep(NA, 4)),
#'                    Visit.Datetime = as.POSIXct(c("2024-05-01", "2024-06-15", "2024-05-01", "2024-07-30")),
#'                    Outcome = c("s1", "s1", "s1", "s1"),
#'                    Nest.Status = rep(NA, 4))
#' nw.filterphenology(data = data, phenology = phenology, mode = "flag")
nw.filterphenology <- function(data, mode, phenology, output = NULL){

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
  names(phen) <- tolower(names(phen))                                           # force lowercase column names
  phen <- phen %>% mutate(species = tolower(species))                           # force species codes into lower if not already
  spp_col <- grep("species", colnames(phen), ignore.case = TRUE)                # get the spp column index


  for (sp in phen[[spp_col]]){

    spp_data <- data %>% filter(Species.Code == sp)                         # Filter the dataset to just the sp of interest
    spp_col <- grep("species", colnames(phen), ignore.case = TRUE)          # get the spp column index
    spp_phen <- phen[phen[[spp_col]] == sp, ]                               # filter phenology data to just the spp of interest

    # Define phenology vector elements by searching for column names
    lay <- spp_phen[[grep("lay", colnames(spp_phen))]]
    inc <- spp_phen[[grep("incubation", colnames(spp_phen))]]
    nestling <- spp_phen[[grep("nestling", colnames(spp_phen))]]
    total <- spp_phen[[grep("total", colnames(spp_phen))]]


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
    ### Select attempts that did not have summary info which would trigger the previous flags
    ### Select checks where nest status != "no" or "in" -- some people enter empty box checks/incomplete nests  which we want to exclude here
    temp <- spp_data %>% filter(is.na(First.Lay.Date) | is.na(Hatch.Date) | is.na(Fledge.Date))  # if no summary dates provided, subset to temp
    temp <- temp %>% filter(Nest.Status != "no" | is.na(Nest.Status))  # remove status "no nest" checks (ie. empy box)
    temp <- temp %>% filter(Nest.Status != "in" | is.na(Nest.Status))  # remove status "incomplete nest" checks

    temp <- temp %>% filter(!is.na(Visit.Datetime))
    temp <- temp %>% group_by(Attempt.ID) %>%
      summarize(min_date = min(Visit.Datetime),
                max_date = max(Visit.Datetime),    # this would do 2nd to last date: nth(sort(Visit.Datetime, decreasing = TRUE), 2),
                date_difference = as.integer(difftime(max_date, min_date, units = "days")))
    flag <- temp %>% filter(date_difference > total) %>% pull(Attempt.ID) %>% unique()

    # Combine both flags
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
