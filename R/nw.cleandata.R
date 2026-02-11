#' Perform Common Cleaning Procedures to NestWatch Data
#'
#' @description NestWatch data are collected by volunteer participants (researchers & the public) and are known to contain some errors.
#' This function provides several common procedures that NestWatch staff have identified to help the user clean NestWatch data for analysis.
#' \strong{The user should consider their research objectives/species life histories and read the full documentation below and in the vignette.}
#'
#' @details NestWatch data in the database should be considered "raw" and the user should consider their system and research objectives when
#' deciding what cleaning procedures to conduct. This function contains 11 cleaning procedures \code{a:k}. \cr
#' \cr
#'     \strong{Cleaning Procedure Details}:
#'       \itemize{
#'        \item \code{a}: Flag/remove attempts with attempts with `Species.Name` entered as a species known to be an obligate brood parasite. These
#'        species do not create their own nests, so this is not the correct interpretation of the nest species. Users may choose to look at these data more
#'        closely if investigating brood parasitism. See the full list or obligate brood parasite species
#'        \href{https://https://engagement-center.github.io/nestwatchR/articles/b_Data-Cleaning.html#parasite-table}{click here}.
#'        \item \code{b}: Flag/remove attempts with  of "no breeding behavior observed" (\code{u3}), "inactive" (\code{i}), and
#'        "not monitored" (\code{n}). These data likely represent nests or nest boxes which never received eggs or were unmonitored. Users may choose
#'        to include these data if looking at habitat or location data without measures of phenology or success.
#'        \item \code{c}: Flag/remove attempts with \code{Outcome} of “invasive species management” (\code{f5}). For these attempts, the participant
#'        chose to remove/alter eggs or nests of invasive species. If the user is interested in analyzing participants’ habits in invasive species
#'        management or using some of the data fields (e.g., clutch size, first lay date), they might consider skipping this method with the
#'        understanding that the outcome code is interpreted as “failure due to human interference” and standard nest survival estimates would
#'        not be meaningful. Note: “Invasive species management” may also be incorrectly chosen by participants trying to indicate nest failure
#'        caused by an invasive species, in which case it would still be prudent to flag or remove such records.
#'        \item \code{d}: Flag/remove attempts if outcome is a failure code (\code{f, f1, f2, f3, f6, f7}) but recorded fledged host young > 0.
#'        In this case, a participant may have either characterized the nest's outcome incorrectly, recorded the presence of fledged host young
#'        incorrectly, or mischaracterized brood parasite young as host. Discerning the true outcome may not be possible. If an attempt
#'        produces any number of fledged host young, the attempt is considered successful.
#'        \item \code{e}: Flag/remove attempt if \code{Outcome} is success (\code{s1}), but recorded fledged host young = 0. Inverse of \code{d}.
#'        \item \code{f}: Flag/remove attempt if # hatched host young > clutch size. This may indicate incorrectly entered data or long lengths of time
#'        between nest checks where summary data was not properly updated. An analyst may choose to review these attempts by looking with caution
#'        at the nest visit data to validate hatched young and clutch sizes.
#'        \item \code{g}: Flag/remove attempt if # fledged host young > # hatched host young. Similar to \code{f}.
#'        \item \code{h}: Flag/remove attempts for which NestWatch failed to identify a region (\code{Subnational.Code == “XX-”}).
#'        Subnational.Code is automatically assigned based on coordinates supplied by the participant. Many attempts identified as XX- likely resulted
#'         from nests being located in water bodies and/or participants entering incorrect coordinates. Consider removing the “XX” attempts if the
#'         coordinates are implausible for the focal species and consider inclusion if analyzing coastal or water-nesting species (e.g., Osprey
#'         nesting on channel markers).
#'        \item \code{i}: Flag/remove attempts where the number of eggs or young decrease and then subsequently increase. This may happen if
#'        a nest fails and a new attempt is started at the same location, which should be two individual nesting attempts. If the analyst is
#'        looking at host response to depredation events or egg dumping you may choose to retain these records.
#'        \item \code{j}: Flag/remove attempts with impossible nesting periods. Incorrect years are sometimes entered between date summary fields
#'        by participants. This may produce impossibly long nesting periods. To account for nesting phenologies in all hemispheres, this procedure
#'        identifies attempts in which (1) Fledge Date - First Lay Date > 365 days, (2) Hatch Date - First Lay Date > 84 days, or (3) Fledge Date
#'         - Hatch Date > 300 days. These dates represent the maximum nest phenological period for any bird species and are not realistic for the
#'        majority of the NestWatch dataset. We encourage users to determine reasonable phenologies for their species of interest and use
#'        \code{\link[nestwatchR:nw.filterphenology]{nw.filterphenology}} to run a finer filter on nest phenology dates by species.
#'        \item \code{k}: Flag/remove attempts where the # days between the first and the last visit are > 365 days. Additional check to identify nest attempts where year portion
#'        of dates between nest visits were likely incorrectly entered. An analyst may choose to review these attempts individually to verify if a typo occurred.
#' }
#'
#' @param data dataframe; A dataframe containing NestWatch data.
#' @param mode \code{"flag"} or \code{"remove"}; A character string defining if the user wants the identified nesting attempts to be flagged with "FLAGGED" in a new column. Or removed from the dataset.
#' @param methods character vector; A vector containing any of the letters \code{a:k} (not case- or order-sensitive), identifying each cleaning procedure to be conducted on the data. See \strong{Details} below and the vignette for details on each procedure.
#' @param output character; An optional character string to custom name the output dataframe.
#'
#' @importFrom stringr str_detect
#' @importFrom stats complete.cases
#' @importFrom stats na.omit
#' @importFrom lubridate year
#' @importFrom lubridate parse_date_time
#' @return a dataframe
#' @export
#'
#' @examples
#' # Load example wren data
#' wrens <- nestwatchR::wren_quickstart
#'
#' # Flag data not meeting procedures e, f, g, and h
#' nw.cleandata(data = wrens, mode = "flag",
#'              methods = c("e", "f", "g", "h"))
#'
#' # Remove data not meeting procedures j or k.
#' nw.cleandata(data = wrens, mode = "remove",
#'              methods = c("j", "k"))
nw.cleandata <- function(data, mode, methods, output = NULL) {

  #####################################                                         # Stops the function if the provided arguments are not correctly provided
  ###   Function Parameters Check   ###
  #####################################

  # Stops Function if 'mode' is not specified.
  if (missing(mode)) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  # Stops Function if 'mode' is invalid.
  if (!mode %in% c("flag", "remove")) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  # Stops Function if 'methods' is invalid.
  if (missing(methods)) {
    stop("Invalid 'methods'. See ?nw.cleandata for descriptions.")
  }
  # If 'methods' contains any invalid characters, stop the function.
  valid_methods <- c(letters[1:12], LETTERS[1:11])
  if (!all(methods %in% valid_methods)) {
    stop("Invalid 'methods'. See ?nw.cleandata for descriptions.")
  }# end of parameter check




  #####################################
  ###        Dataframe Setup        ###
  #####################################

  # Initalize Column Names
  Attempt.ID <- Visit.Datetime <- max_date <- min_date <- Flagged.Attempt <- Species.Code <- Outcome <- NULL
  Young.Total <- Clutch.Size <- Young.Fledged <- Subnational.Code <- date_diff <- First.Lay.Date <- NULL
  Fledge.Date <- Hatch.Date <- Unacceptable <- Live.Host.Young.Count <- Host.Eggs.Count <- NULL
  date_span <- all_dates_match_year <- scientific_name <- family <- parasite <- species_code <- NULL
  species_code <- category <- report_as <- Species.Code <- common_name <- NULL

  # Prep dataframe
  data <- data %>% mutate(Flagged.Attempt = NA) %>%                           # Make new column to hold flag code
    relocate(Flagged.Attempt, .before = Attempt.ID)          # Reorder column to beginning
  data$First.Lay.Date <- as.Date(data$First.Lay.Date)                           # Make all date-times proper formats
  data$Fledge.Date    <- as.Date(data$Fledge.Date)
  data$Hatch.Date     <- as.Date(data$Hatch.Date)
  data$Visit.Datetime <- lubridate::parse_date_time(data$Visit.Datetime, orders = c("Y-m-d H:M:S", "Y-m-d")) # datetime handling fix

  # Force methods to be lowercase
  methods <- tolower(methods)

  #####################################
  ###    Individual Methods         ###
  #####################################


  # Loop through each individual method.
  if (all(methods %in% letters[1:12])) {
    message("... Beginning to identify nesting attempts that do not meet the criteria. This may take a minute ...")


    # Start of methods loop:
    for (m in methods) {
      if (m == "a") {
        # Code block for method "a"
        # A. Flag brood parasite "nests"

        # Get list of obligate brood parasite species codes
        # Define genera/families
        parasite_genera <- c("Heteronetta", "Molothrus", "Pachycoccyx", "Microdynamis", "Eudynamys",
                             "Scythrops", "Urodynamis", "Chrysococcyx", "Cacomantis",
                             "Surniculus", "Cercococcyx", "Hierococcyx", "Cuculus",
                             "Clamator", "Vidua", "Anomalospiza")
        parasite_family <- c("Indicatoridae")

        # Regex patterns
        genera_pattern <- paste(parasite_genera, collapse = "|")
        family_pattern <- paste(parasite_family, collapse = "|")

        # Prepare data
        parasites <- auk::get_ebird_taxonomy() %>%
          mutate(parasite = ifelse(grepl(genera_pattern, scientific_name), TRUE, NA)) %>%
          mutate(parasite = ifelse(grepl(family_pattern, family), TRUE, parasite)) %>%
          filter(parasite == TRUE) %>% pull(species_code)

        # Flag
        toflag <- data %>% filter(Species.Code %in% parasites) %>% pull(Attempt.ID) %>% unique()
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"

      } else if (m == "b") {
        # Code block for method "b"
        # B. Flag outcomes of "no breeding behavior observed" (u3), "inactive" (i), and "not monitored" (n).
        toflag <- data %>% filter(Outcome %in% c("u3", "i", "n")) %>% pull(Attempt.ID) %>% unique()
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"

      } else if (m == "c") {
        # Code block for method "c"
        # C. Flag outcome "invasive spp management" (f5).
        toflag <- data %>% filter(Outcome == "f5") %>% pull(Attempt.ID) %>% unique()
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"

      } else if (m == "d") {
        # Code block for method "d"
        # D. Flag attempt if outcome is fail (f, f1, f2, f3, f6, f7), but recorded fledged young > 0.
        toflag <- data %>% filter(Outcome %in% c("f", "f1", "f2", "f3", "f6", "f7")) %>%
          filter(Young.Fledged > 0) %>%
          pull(Attempt.ID) %>% unique()
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"

      } else if (m == "e") {
        # Code block for method "e"
        # E. Flag attempt if outcome is success (s1), but recorded fledged young = 0.
        toflag <- data %>% filter(Outcome == "s1") %>%
          filter(Young.Fledged == "0") %>%
          pull(Attempt.ID) %>% unique()
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"

      } else if (m == "f") {
        # Code block for method "f"
        # F. Flag attempt if # hatched young > clutch size.
        toflag <- data %>% filter(Young.Total > Clutch.Size) %>% pull(Attempt.ID) %>% unique()
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"

      } else if (m == "g") {
        # Code block for method "g"
        # G. Flag attempt if # fledged > # hatched.
        toflag <- data %>% filter(Young.Fledged > Young.Total) %>% pull(Attempt.ID) %>% unique()
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"

      } else if (m == "h") {
        # Code block for method "h"
        # H. Flag attempts that occur in international waters (likely incorrect coords, Subnational code = XX-)
        toflag <- data %>% filter(Subnational.Code == "XX-") %>% pull(Attempt.ID) %>% unique()
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"

      } else if (m == "i") {
        # Code block for method "i"
        # I. Flag attempts where the number of eggs or young decrease and then increase

        # Function to check for egg or young count changes
        check_unacceptable <- function(counts) {
          # Remove NA values for the purpose of comparisons
          non_na_counts <- counts[!is.na(counts)]

          # Identify indices where the counts decrease
          decrease_indices <- which(diff(non_na_counts) < 0)

          # If no decrease, it's acceptable
          if (length(decrease_indices) == 0) {
            return(FALSE)
          }

          # Check if any count after the first decrease is greater than any count before
          first_decrease <- decrease_indices[1]
          before_decrease <- non_na_counts[1:first_decrease]
          after_decrease <- non_na_counts[(first_decrease + 1):length(non_na_counts)]

          # Compare the maximum of counts before the decrease with the counts after the decrease
          max_before <- max(before_decrease)
          any(after_decrease > max_before)
        } # end count function


        # Apply the function to each Attempt for Egg Counts
        temp <- data %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime) %>%
          summarize(Unacceptable = check_unacceptable(Host.Eggs.Count),  # apply function for eggs
                    .groups = "drop")
        toflag <- temp %>% filter(Unacceptable) %>% pull(Attempt.ID) %>% unique()      # flagged eggs count attempts
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"

        # Apply the function to each Attempt for Young Counts
        temp <- data %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime) %>%
          summarize(Unacceptable = check_unacceptable(Live.Host.Young.Count), # apply function for young
                    .groups = "drop")
        toflag <- temp %>% filter(Unacceptable) %>% pull(Attempt.ID) %>% unique()      # flagged young count attempts
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"



      } else if (m == "j") {
        # Code block for method "j"
        # I. Flag attempts with nest periods > absolute max of any bird
        #    Stepping through different nest stages looking for if any summary date data was provided to work off of
        # Find attempts where Fledge - Lay > 365 (Snowy Albatross)
        # Look for attempts with both Lay and Fledge dates
        phen1 <- unlist(data[complete.cases(str_detect(data$First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")), "Attempt.ID"])
        phen2 <- unlist(data[complete.cases(str_detect(data$Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")), "Attempt.ID"])
        shared <- intersect(phen1, phen2)
        temp <- data %>% filter(Attempt.ID %in% shared)
        # Calculate date diff, filter
        temp <- temp %>% mutate(date_diff = (Fledge.Date - First.Lay.Date))
        temp <- temp %>% filter(date_diff > 365)
        toflag <- unique(temp$Attempt.ID)
        # Find attempts where Hatch - Lay > 84 (Southern Brown Kiwi)
        # Look for attempts with both Lay and Fledge dates
        phen1 <- unlist(data[complete.cases(str_detect(data$First.Lay.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")), "Attempt.ID"])
        phen2 <- unlist(data[complete.cases(str_detect(data$Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")), "Attempt.ID"])
        shared <- intersect(phen1, phen2)
        temp <- data %>% filter(Attempt.ID %in% shared)
        # Calculate date diff, filter
        temp <- temp %>% mutate(date_diff = (Hatch.Date - First.Lay.Date))
        temp <- temp %>% filter(date_diff > 84)
        toflag <- c(toflag, unique(temp$Attempt.ID))
        toflag <- unique(toflag)
        # Find attempts where Fledge - Hatch > 300 (Snowy Albatross)
        # Look for attempts with both Lay and Fledge dates
        phen1 <- unlist(data[complete.cases(str_detect(data$Hatch.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")), "Attempt.ID"])
        phen2 <- unlist(data[complete.cases(str_detect(data$Fledge.Date, "\\b\\d{4}-\\d{2}-\\d{2}\\b")), "Attempt.ID"])
        shared <- intersect(phen1, phen2)
        temp <- data %>% filter(Attempt.ID %in% shared)
        # Calculate date diff, filter
        temp <- temp %>% mutate(date_diff = (temp$Fledge.Date - temp$Hatch.Date))
        temp <- temp %>% filter(date_diff > 300)
        toflag <- c(toflag, unique(temp$Attempt.ID))
        toflag <- unique(toflag)
        # Run filter
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"
        rm(temp)

      } else if (m == "k") {
        # J. Flag attempts where the # days between the first check and the last check are > 365 days (Snowy Albatross nest span)
        # or where year of summary dates is not in the check year
        temp <- data %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime) %>%                             # group by attempt
                         mutate(date_span = as.numeric(max(Visit.Datetime) - min(Visit.Datetime))) %>%    # calc max check date span
                         filter(date_span >= 365) %>%                                                     # filter to long attempts
                         pull(Attempt.ID) %>% unique()                                                    # get unique attempt.ids to flag

        # Flag attempts
        toflag <- c(temp)
        toflag <- unique(toflag)
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"
        rm(temp)

      # Unused, included better in nw.filterphenology()
      # } else if (m == "l") {
      #   # L. Trim trailing nest checks post fledge/fail
      #   # Create a column to designate if each attempt had or did not have live young at any point
      #   temp <- data %>% group_by(Attempt.ID) %>% mutate(had_young = any(Live.Host.Young.Count > 0, na.rm = TRUE))
      #
      #   # If young were present:
      #   temp_1 <- temp %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime) %>%
      #     mutate(is_positive = !is.na(Live.Host.Young.Count) & Live.Host.Young.Count > 0,
      #            first_zero_after_positive = which(Live.Host.Young.Count == 0 & cumsum(is_positive) > 0)[1]) %>% # finds first 0 young after possitive
      #     mutate(keep = row_number() <= first_zero_after_positive) %>%    # label rows to keep as TRUE else FALSE
      #     filter(keep == "FALSE") %>% pull(Visit.ID)
      #
      #   # If young were not present:
      #   temp_2 <- temp %>% filter(had_young == FALSE)
      #   temp_2 <- temp_2 %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime) %>%      # group checks by attempt and arrange by date
      #     mutate(is_positive = !is.na(Host.Eggs.Count) & Host.Eggs.Count > 0,
      #            first_zero_after_positive = which(Host.Eggs.Count == 0 & cumsum(is_positive) > 0)[1]) %>% # finds first 0 young after possitive
      #     mutate(keep = row_number() <= first_zero_after_positive) %>%    # label rows to keep as TRUE else FALSE
      #     filter(keep == "FALSE") %>% pull(Visit.ID)
      #
      #   # Flag bad checks
      #   toflag <- c(temp_1, temp_2)
      #   toflag <- unique(toflag)
      #   rows <- which(data$Visit.ID %in% toflag)
      #   data$Flagged.Attempt[rows] <- "FLAGGED"
      #   rm(temp_1, temp_2)


      } # end last method code chunk






    }
  }
  #####################################
  ###        Mode == "flag"         ###
  #####################################


  # Prep for and Export resulting dataframe
  pos <- 1
  envir = as.environment(pos)

  # If mode == "flag"
  if (mode == "flag") {
    # Export resulting dataframe
    if (is.null(output)) {
      cleaned.data <- NULL
      assign("cleaned.data", data, envir = envir)
    } else {
      assign(paste0(output), data, envir = envir)
    }
    message("... Identified nesting attempts have been noted with 'FLAGGED' in the new column 'Flagged.Attempt'.")
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
      assign("cleaned.data", data, envir = envir)
    } else {
      assign(paste0(output), data, envir = envir)
    }
    message("... Identified nesting attempts have been removed from the new dataset.")
  }
} ##### end whole function

