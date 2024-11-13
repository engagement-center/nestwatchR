#' Perform Common Cleaning Procedures to NestWatch Data
#'
#' @description NestWatch data are collected by volunteer participants (researchers & the public) and are known to contain some errors.
#' This function provides several common procedures that NestWatch staff have identified to help the user clean NestWatch data for analysis.
#' \strong{The user should consider their research objectives/species life histories and read the full documentation below and in the vignette.}
#'
#' @details NestWatch data in the database should be considered "raw" and the user should consider their system and research objectives when
#' deciding what cleaning procedures to conduct. This function contains 10 cleaning procedures \code{a:j}. \cr
#' \cr
#'     \strong{Cleaning Procedure Details}:
#'       \itemize{
#'        \item \code{a}: Flag/remove attempts with \code{Species.Name} is "Brown-headed Cowbird" as they are nest parasites and
#'        do not create their own nests. This is not the correct interpretation of the nest species. Users may choose to look at these data more
#'        closely if investigating brood parasitism.
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
#'        \item \code{i}: Flag/remove attempts with impossible nesting periods. Incorrect years are sometimes entered between date summary fields
#'        by participants. This may produce impossibly long nesting periods. To account for nesting phenologies in all hemispheres, this procedure
#'        identifies attempts in which (1) Fledge Date - First Lay Date > 365 days, (2) Hatch Date - First Lay Date > 84 days, or (3) Fledge Date
#'         - Hatch Date > 300 days. These dates represent the maximum nest phenological period for any bird species and are not realistic for the
#'        majority of the NestWatch dataset. We encourage users to determine reasonable phenologies for their species of interest and use
#'        \code{\link[nestwatchR:nw.filterphenology]{nw.filterphenology}} to run a finer filter on nest phenology dates.
#'        \item \code{j}: Flag/remove attempts where the # days between the first and the last visit are > 365 days. Additional check to identify
#'        nest attempts where year portion of dates between nest visits were likely incorrectly entered. A user may choose to review these attempts
#'        individually to verify if a typo occurred.
#' }
#'
#' @param data dataframe; A dataframe containing NestWatch data.
#' @param mode \code{"flag"} or \code{"remove"}; A character string defining if the user wants the identified nesting attempts to be flagged with "FLAGGED" in a new column. Or removed from the dataset.
#' @param methods character vector; A vector containing any of the letters \code{a:j} (not case- or order-sensitive), identifying each cleaning procedure to be conducted on the data. See \strong{Details} below and the vignette for details on each procedure.
#' @param output character; An optional character string to custom name the output dataframe.
#'
#' @importFrom stringr str_detect
#' @importFrom stats complete.cases
#' @return a dataframe
#' @export
#'
#' @examples
#' # Load example wren data
#' wrens <- nestwatchR::wren_quickstart
#'
#' # Flag data not meeting procedures e, f, g, and h
#' nw.cleandata(data = wrens, mode = "flag",
#'              methods = c("a", "b", "c", "d", "e", "f", "g", "h"))
#'
#' # Remove data not meeting procedures i or j.
#' nw.cleandata(data = wrens, mode = "remove",
#'              methods = c("i", "j"))
nw.cleandata  <- function(data, mode, methods, output = NULL) {

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
  valid_methods <- c(letters[1:10], LETTERS[1:10])
  if (!all(methods %in% valid_methods)) {
    stop("Invalid 'methods'. See ?nw.cleandata for descriptions.")
  }# end of parameter check




  #####################################
  ###        Dataframe Setup        ###
  #####################################

  # Initalize Column Names
  Attempt.ID <- Visit.Datetime <- max_date <- min_date <- Flagged.Attempt <- Species.Code <- Outcome <- NULL
  Young.Total <- Clutch.Size <- Young.Fledged <- Subnational.Code <- date_diff <- First.Lay.Date <- NULL
  Fledge.Date <- Hatch.Date <- NULL

  # Prep dataframe
  data <- data %>% mutate(Flagged.Attempt = NA) %>%                           # Make new column to hold flag code
    relocate(Flagged.Attempt, .before = Attempt.ID)          # Reorder column to beginning
  data$First.Lay.Date <- as.Date(data$First.Lay.Date)                           # Make all date-times proper formats
  data$Fledge.Date    <- as.Date(data$Fledge.Date)
  data$Hatch.Date     <- as.Date(data$Hatch.Date)
  data$Visit.Datetime <- as.POSIXct(data$Visit.Datetime, format = "%Y-%m-%d %H:%M:%S", tz = "")

  # Force methods to be lowercase
  methods <- tolower(methods)

  #####################################
  ###    Individual Methods         ###
  #####################################


  # Loop through each individual method.
  if (all(methods %in% letters[1:10])) {
    message("... Beginning to identify nesting attempts that do not meet the criteria. This may take a minute ...")


    # Start of methods loop:
    for (m in methods) {
      if (m == "a") {
        # Code block for method "a"
        # A. Flag BHCO "nests"
        toflag <- data %>% filter(Species.Code == 'bnhcow') %>% pull(Attempt.ID) %>% unique()
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

      } else if (m == "j") {
        # J. Flag attempts where the #days between the first check and the last check are < 365 days (Snowy Albatross nets span)
        temp <- data %>% select(c("Attempt.ID", "Visit.Datetime"))
        temp$Visit.Datetime <- gsub(" .*", "", temp$Visit.Datetime)
        temp <- temp %>% filter(!is.na(Visit.Datetime))
        # Convert to unix dates
        temp$Visit.Datetime <- as.numeric(as.Date(temp$Visit.Datetime))
        temp <- group_by(temp, Attempt.ID)
        temp <- temp %>% summarise(min_date = min(Visit.Datetime), max_date = max(Visit.Datetime))
        temp <- temp %>% mutate(date_diff = as.numeric(max_date - min_date))
        toflag <- temp %>% filter(temp$date_diff > 365) %>% pull("Attempt.ID") %>% unique()
        rows <- which(data$Attempt.ID %in% toflag)
        data$Flagged.Attempt[rows] <- "FLAGGED"
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
      assign("cleaned.data", data, envir = envir)
    } else {
      assign(paste0(output), data, envir = envir)
    }
    message("... Identified nesting attempts have been removed from the new dataset.")
  }
} ##### end whole function

