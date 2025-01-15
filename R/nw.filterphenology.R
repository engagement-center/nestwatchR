#' Filter Nesting Attempts Based on Expected Nest Phenological Periods
#'
#' @description To provide additional checks on data quality before analysis, an analyst may want to filter out nesting attempts which were
#' recorded to have nesting phenologies of unexpectedly long lengths. This function allows an analyst to specify species-specific values for nest phenologies
#' using expected days in the lay, incubation, nestling, and total nesting periods. By defining these allowable time frames an analyst can flag or
#' remove "run-on" nest attempts or nests which do not match the specie's life history.
#'
#'
#' @param data dataframe; A dataframe of merged NestWatch attempt and visits data. Date columns must have class \code{Date}
#' and Datetime columns must have class \code{POSIXct}.
#' @param mode \code{"flag"} or \code{"remove"}; A character string defining if the analyst wants the identified nesting attempts to be
#' flagged with "FLAGGED" in a new column, or removed from the dataset.
#' @param max_phenology dataframe; A simple dataframe with one row for each species of interest and the following column structure:
#'  \itemize{
#'   \item \strong{Lay}: The number of days representing the maximum expected laying period for each species.
#'   \item \strong{Incubation}: The number of days representing the maximum expected incubation period (between clutch complete and hatch) for each species.
#'   \item \strong{Nestling}: The number of days representing the maximum expected nesting period (hatch and last fledge) for each species.
#'   \item \strong{Total Nesting Period}: The number of days representing the maximum expected nesting period (spanning first initial nest building to last fledge) for each species.
#'   }
#' @param trim_to_active logical; TRUE or FALSE indicating if nest check data should be trimmed to include only those rows where the nest was active
#' (in build, lay, incubation, presence of life young). The first check observing a fledge or fail event is retained, but subsequent checks would be
#' targeted for flag/removal. Flag/removal follows \code{mode}.
#' @param output character; An optional character string to custom name the output dataframe
#'
#' @details This function calculates the number of days between \code{First.Lay.Date}, \code{Hatch.Date}, and \code{Fledge.Date} date values, comparing
#' each to the user-provided acceptable date spans for each nest phase. Not all attempts contain these summary dates. An analyst may choose to explore
#' \link[nestwatchR]{nw.estclutchsize}, \link[nestwatchR]{nw.estfirstlay}, \link[nestwatchR]{nw.esthatch}, and \link[nestwatchR]{nw.estfledge} functions
#' to estimate these summary dates from the individual nest visit data prior to using this function. As an additional check, this function calculates
#' the date span between the first occurrence of an incomplete nest, complete nest, observed egg, or observed young and last active date (either the last
#' observation of eggs/live young or the first 0-egg/young count indicating fail/fledge) and compares this date span to the user-provided "total nesting
#' period" value.
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
#' phenology <- data.frame(lay = c(7),          # max observed
#'                         incubation = c(20),  # mean plus some extra
#'                         nestling = c(20),    # mean plus some extra
#'                         total = c(50))       # mean plus some extra
#'
#'
#' # Simplified NestWatch dataset with nest summary dates
#' # Attempts 3 & 4 should be flagged (too long in incubation and nestling phases respectively)
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
#' # Simplified NestWatch dataset without nest summary dates
#' # (will look at total nest attempt duration from visit dates)
#' # Attempt "2" should be flagged as being too long.
#' data <- data.frame(Attempt.ID = c("1", "1", "2", "2"),
#'                    Species.Code = rep("carwre", 4),
#'                    First.Lay.Date = as.Date(rep(NA, 4)),
#'                    Hatch.Date = as.Date(rep(NA, 4)),
#'                    Fledge.Date = as.Date(rep(NA, 4)),
#'                    Visit.Datetime =
#'                    as.POSIXct(c("2024-05-01", "2024-06-15",
#'                                 "2024-05-01", "2024-07-30")),
#'                    Outcome = c("s1", "s1", "s1", "s1"),
#'                    Nest.Status = rep(NA, 4))
#' nw.filterphenology(data = data, phenology = phenology, mode = "flag")
nw.filterphenology <- function(data, mode, max_phenology, trim_to_active, output = NULL){

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
  if (!is.data.frame(max_phenology) || ncol(max_phenology) != 5) {
    stop("Argument 'phenology' must be a dataframe with 5 columns. See ?nw.filterphenology() for details.")
  }
  # Stops function if phenology dataframe does not have the correct column names.
  required_cols <- c("species", "lay", "incubation", "nestling", "total")
  missing_cols <- required_cols[!sapply(required_cols, function(col) {  # Check if all required columns are present, ignoring case
    any(grepl(col, colnames(max_phenology), ignore.case = TRUE))
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
  Flagged.Attempt <- Flagged.Check <- Attempt.ID <- Species.Code <- First.Lay.Date <- Hatch.Date <- Fledge.Date <- NA
  Visit.Datetime <- max_date <- min_date <- date_difference <- Live.Host.Young.Count <- Host.Egg.Count <- NA
  toflag <- species <- Host.Eggs.Count <- CumulativeMax <- is_positive <- first_zero_after_positive <- NA
  keep <- Visit.ID <- had_young <- Nest.Status <- Host.Eggs.Present.Uncounted <- NA
  first_valid_row <- period_length <- Live.Host.Young.Present.Uncounted <- NA

  # Prep dataframe
  data <- data %>% mutate(Flagged.Attempt = NA, Flagged.Check = NA) %>%         # Make new columns to hold flag codes
    relocate(Flagged.Attempt, .before = Attempt.ID) %>%          # Reorder columns to beginning
    relocate(Flagged.Check, .before = Attempt.ID)

  phen <- max_phenology                                                         # shorter name
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




    ################################################################
    ###        Flag Long Attempts based on whole period          ###
    ################################################################

    # Temp trim proceeding empty nest checks from each attempt
    temp <- spp_data %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime) %>%      # group checks by attempt and arrange by date
      mutate(CumulativeMax = cummax(Host.Eggs.Count)) %>%       # make column for cumulative eggs
      filter(!(Host.Eggs.Count == 0 & CumulativeMax == 0)) %>%  # remove checks up until egg count != 0
      select(-CumulativeMax)                                    # remove temporary column

    # Trim trailing empty nest checks after fledge/fail
    # Create a column to designate if each attempt had or did not have live young at any point
    temp <- data %>% group_by(Attempt.ID) %>% mutate(had_young = any(Live.Host.Young.Count > 0, na.rm = TRUE))

    # If young were present:
    temp_1 <- temp %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime) %>%
      mutate(is_positive = !is.na(Live.Host.Young.Count) & Live.Host.Young.Count > 0,
             first_zero_after_positive = which(Live.Host.Young.Count == 0 & cumsum(is_positive) > 0)[1]) %>% # finds first 0 young after positive
      mutate(keep = row_number() <= first_zero_after_positive) %>%    # label rows to keep as TRUE else FALSE
      filter(keep == "FALSE") %>% pull(Visit.ID)

    # If young were not present:
    temp_2 <- temp %>% filter(had_young == FALSE)
    temp_2 <- temp_2 %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime) %>%      # group checks by attempt and arrange by date
      mutate(is_positive = !is.na(Host.Eggs.Count) & Host.Eggs.Count > 0,
             first_zero_after_positive = which(Host.Eggs.Count == 0 & cumsum(is_positive) > 0)[1]) %>% # finds first 0 young after positive
      mutate(keep = row_number() <= first_zero_after_positive) %>%    # label rows to keep as TRUE else FALSE
      filter(keep == "FALSE") %>% pull(Visit.ID)

    # Flag trailing checks
    to_flag_visits <- c(temp_1, temp_2)                               # combine temp 1 and 2

    # Remove preceding checks
    temp_3 <- spp_data %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime) %>%
      mutate(first_valid_row = which( # The nest is a being built (status == "in" or "cn")
        (Nest.Status == "in" | Nest.Status == "cn") |
          # The nest has contents (eggs or young counted/present)
          (Host.Eggs.Count != 0 & !is.na(Host.Eggs.Count)) |
          (Host.Eggs.Present.Uncounted != 0 & !is.na(Host.Eggs.Present.Uncounted)) |
          (Live.Host.Young.Count != 0 & !is.na(Live.Host.Young.Count)) |
          (Live.Host.Young.Present.Uncounted != 0 & !is.na(Live.Host.Young.Present.Uncounted)))[1]) %>%  # Find the first egg or young in each group
      filter(row_number() < first_valid_row) %>%
      pull(Visit.ID) # grab visit.id of those checks before nest is active

    # Combine all three sets of visits to flag
    to_flag_visits <- c(temp_1, temp_2, temp_3)


    # Calculate active nesting period
    temp <- spp_data %>% group_by(Attempt.ID) %>%
      filter(!Visit.ID %in% to_flag_visits) %>%  # look at just active nesting period (build included)
      mutate(period_length = as.numeric(max(Visit.Datetime) - min(Visit.Datetime))) %>%  # cal nesting period
      filter(period_length > total)                                                      # filter to long attempts
    toflag <- temp %>% pull(Attempt.ID) %>% unique()  # create vector to hold unique attempt.IDs to flag bc too long



    ############################################################
    ###        Flag Long Attempts based on benchmarks        ###
    ############################################################

    # Find attempts where Hatch - Lay > user input
    if(!is.na(lay) & !is.na(inc)){ # check to make sure both vars are provided
      H_L <- lay + inc
      temp <- spp_data %>% filter(str_detect(First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
        filter(str_detect(Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
      temp <- temp %>% filter(as.integer(Hatch.Date - First.Lay.Date) > H_L)
      flag <- unique(temp$Attempt.ID)
      toflag <- unique(c(toflag, flag))
    }

    # Find attempts where Fledge - Hatch > user input
    if(!is.na(nestling)){ # check to make var is provided
      H_F <- nestling
      temp <- spp_data %>% filter(str_detect(Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
        filter(str_detect(Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
      temp <- temp %>% filter(as.integer(Fledge.Date - Hatch.Date) > H_F)
      flag <- unique(temp$Attempt.ID)
      toflag <- unique(c(toflag, flag))
    }

    # Find attempts where Fledge - Lay > user input
    if(!is.na(lay) & !is.na(inc) & !is.na(nestling)){ # check to make sure all vars are provided
      L_F <- lay + inc + nestling
      temp <- spp_data %>% filter(str_detect(First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")) %>%
        filter(str_detect(Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b"))
      temp <- temp %>% filter(as.integer(Fledge.Date - First.Lay.Date) > L_F)
      flag <- unique(temp$Attempt.ID)
      toflag <- unique(c(toflag, flag))
    }





    ##################################
    ###        Flag Attempts       ###
    ##################################

    # Flag in original dataset
    # Flag long attempts
    rows <- which(data$Attempt.ID %in% toflag)
    data$Flagged.Attempt[rows] <- "FLAGGED"


    if(isTRUE(trim_to_active)){
      # Flag non-active checks
      rows <- which(data$Visit.ID %in% to_flag_visits)
      data$Flagged.Check[rows] <- "FLAGGED"
    } # end if trimming is set to TRUE

    if(isFALSE(trim_to_active)){
      # Remove unused column
      data <- data %>% select(-Flagged.Check)
    } # end if trimming is set to FALSE

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
    message("... Identified nesting attempts have been noted with 'FLAGGED' in the new dataset in column 'Flagged.Attempt' & 'Flagged.Check'.")
  } else {


    #####################################
    ###       Mode == "remove"        ###
    #####################################

    # if mode was remove
    # Filter out any nest attempts that were flagged and remove column used to flag
    data <- data[!grepl("FLAGGED", data$Flagged.Attempt), ]
    data <- data %>% select(-Flagged.Attempt)

    data <- data[!grepl("FLAGGED", data$Flagged.Check), ]
    data <- data %>% select(-Flagged.Check)


    # Export resulting dataframe
    if (is.null(output)) {
      filtered.data <- NULL
      assign("filtered.data", data, envir = envir)
    } else {
      assign(paste0(output), data, envir = envir)
    }
    message("... Identified nesting attempts have been removed from the new dataset.")
  }



} # end function
