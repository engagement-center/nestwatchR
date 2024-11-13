#' Estimate Hatch Date for NestWatch Data
#'
#' @description Hatch Date data, like first lay date is important for investigating changes in nesting phenology and success over time. These data
#' in the NestWatch dataset are provided by participants, and the input of these dates may be overlooked by the participants. If not explicitly
#' provided by the participant, hatch date may be estimated based off first lay date or fledge Date. This function checks, in order, if the
#' Young.Total were recorded or if the nest fledged (positive indication of hatching), then if a hatch date is available, and finally if a
#' known (non-estimated) fledge date is available. If these requirements are met, the function uses user-input nest phenology timeframes to
#' estimate hatch date date. Estimated dates are denoted with a \code{1} in \code{data$Hatch.Date.Estimated}.
#'
#' @details Precision of the hatch date estimates depend on what data is used to estimate the values from (and if those values are estimated). Users
#' should consider if they want to estimate hatch dates based on estimated lay dates. The function estimates based off of first lay date if available,
#' and calculates the number of egg-laying-days based on clutch size (if available, or user-provided average clutch size) and eggs-per-day of the
#' species. If lay dates are not available, the function estimates hatch from known fledge dates (Fledge.Date.Estimated == 0) by subtracting the
#' user-provided nestling period length. Due to the uncertainly of sighting and aging fledged young, this function does not estimate hatch date
#' from estimated fledge dates.
#'
#'
#' @param data dataframe; A dataframe containing merged NestWatch attempts and visits data.
#' @param phenology dataframe; A dataframe one row of phenological data for each species to be estimated. Data columns as follows:
#'    \itemize{
#'     \item \code{Species}: Species Codes for each species to be estimated.
#'     \item \code{Clutch.Size}: Average clutch size for each species.
#'     \item \code{Eggs.per.Day}: Average number of eggs laid per day by each species.
#'     \item \code{Incubation}: Average number of days spent in incubation (days between clutch complete and hatch).
#'     \item \code{Nestling}: Average number of days spent in nestling period (days between hatch and fledge).
#'     \item \code{Total}: Average number of days between first lay date and fledge date.
#'     }
#' @param output character; An optional character string to custom name the output dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples
#' # Simplified NestWatch data with missing data
#' # All hatch dates should be 2024-05-20
#' data <- data.frame(Attempt.ID = c("1", "2", "3"),
#'                    Species.Code = rep("carwre", 3),
#'                    First.Lay.Date = as.Date(c("2024-05-01", "2024-05-02", NA)),
#'                    Hatch.Date = as.Date(rep(NA, 3)),
#'                    Fledge.Date = as.Date(c(NA, NA, "2024-06-02")),
#'                    Fledge.Date.Estimated = c(NA, NA, 0),
#'                    Clutch.Size = c(NA, 3, NA),
#'                    Young.Total = c(4, 3, 4),
#'                    Outcome = c("s1", "s1", "s1"))
#'
#' # Create phenology dataframe
#' phenology <- data.frame(Species = c("carwre"),
#'                         Clutch.Size  = c(4),
#'                         Eggs.per.Day = c(1),
#'                         Incubation   = c(16),
#'                         Nestling     = c(13),
#'                         Total = c(40))
#'
#' # Run function
#' nw.esthatch(data = data, phenology = phenology)
nw.esthatch <- function(data, phenology, output = NULL) {

  ###########################
  ####  Check arguments  ####
  ###########################

  # Check the dataframe is merged NW data
  if (missing(data)){
    stop("Augument 'data' must be a dataframe of merged NestWatch attempts and visits data.")
  }
  if (all(!(c("Species.Code", "Visit.ID") %in% names(data)))){
    stop("Augument 'data' must be a dataframe of merged NestWatch attempts and visits data.")
  }
  # Check phenology is a dataframe, has correct column names
  if(!is.data.frame(phenology)){
    stop("Augument 'phenology' must be a dataframe, see ?nw.estfirstlay() for details.")
  }
  needed_columns <- c("Species", "Clutch.Size", "Eggs.per.Day", "Incubation", "Nestling", "Total")
  if (!all(tolower(needed_columns) %in% tolower(colnames(phenology)))) {
    stop("Augument 'phenology' must be a dataframe, see ?nw.estfirstlay() for details.")
  }

  ###########################
  ####  Setup            ####
  ###########################
  # Initiate column names
  Species <- Species.Code <- Attempt.ID <- Eggs.per.Day <- Incubation <- Netsling <- Clutch.Size <- NULL
  Hatch.Date <- Fledge.Date <- Species <- Days.of.Lay <- First.Lay.Date <- Young.Total <- Outcome <- NULL
  First.Lay <- Clutch <- Hatch <- Fledge <- Fledge.Date.Estimated <- NULL


  ###########################
  ####  Function         ####
  ###########################

  # Force species column name to lowercase if upper
  if ("Species" %in% colnames(phenology)) {
    colnames(phenology)[colnames(phenology) == "Species"] <- "species"
  }

  message("... Estimating hatch dates, this may take some time ...")

  for (s in phenology$species) {

    # Define phenology vector elements by searching for column names
    spp_phen <- phenology %>% filter(species == s)
    clutch.size <- spp_phen[[grep("clutch.size", colnames(spp_phen), ignore.case = TRUE)]]
    eggs.per.day <- spp_phen[[grep("eggs.per.day", colnames(spp_phen), ignore.case = TRUE)]]
    inc <- spp_phen[[grep("incubation", colnames(spp_phen), ignore.case = TRUE)]]
    nestling <- spp_phen[[grep("nestling", colnames(spp_phen), ignore.case = TRUE)]]
    total <- spp_phen[[grep("total", colnames(spp_phen), ignore.case = TRUE)]]


    # Filter to a single species
    sp_data <- data %>% filter(Species.Code == s & is.na(Hatch.Date))
    # If this subset contains data, continue (there may not be these cases), if not skip and display message
    if (nrow(sp_data) == 0) {message(paste0("Output Note: Either no Attempts exist for species ", shQuote(s, type = "cmd"), ", or all Attempts already contain values for 'Hatch.Date'."))}
    if (nrow(sp_data) > 0) {

    # Subset to Attempts that had # young >0 or if it fledged
    subset <- sp_data %>% filter(Young.Total > 0 | Outcome == "s1")
    # If this subset contains data, continue (there may not be these cases), if not skip and display message
    if (nrow(subset) == 0) {message(paste0("Output Note: No Hatch Dates were estimated for species ", shQuote(s, type = "cmd"), " as no attempts were successful or had recorded young."))}
    if (nrow(subset) > 0) {


    ########################################
    ##    No Hatch Date, Yes 1st Lay      ##
    ########################################

    # For attempts with clutch sizes
    temp <- subset %>% filter(!is.na(First.Lay.Date) & !is.na(Clutch.Size))       # filter to having first lay dates and clutch size
    if (nrow(temp) > 0) {                                          # If this subset contains data - continue, if not skip
    temp <- temp %>% group_by(Attempt.ID) %>%                                     # group by attempt id
      summarise(First.Lay = mean(First.Lay.Date),                  # get the first lay date and clutch size for each attempt
                Clutch = mean(Clutch.Size)) %>%
      mutate(Hatch = (First.Lay +                                  # hatch = first lay + clutch*egg/day + avg inc
                        (Clutch * eggs.per.day) + inc -1))
    temp <- temp %>% select(Attempt.ID, Hatch)
    }

    # For attempts with no clutch size
    temp1 <- subset %>% filter(!is.na(First.Lay.Date) & is.na(Clutch.Size))       # filter to having first lay dates but no clutch size
    # If this subset contains data, continue (there may not be these cases)
    if (nrow(temp1) > 0) {
      temp1 <- temp1 %>% group_by(Attempt.ID) %>%                               # group by attempt id
                         summarise(First.Lay = mean(First.Lay.Date)) %>%        # get the first lay date and clutch size for each attempt
                         mutate(Hatch = (First.Lay +                            # hatch = first lay + avg clutch*egg/day + avg inc
                                        (clutch.size * eggs.per.day) + inc - 1))
      temp1 <- temp1 %>% select(Attempt.ID, Hatch)
    }


    #################################################
    ##    No Hatch Date, No Lay, Fledge Known      ##
    #################################################

    # filter subset to no lay, but fledge date known (not estimated)
    temp2 <- subset %>% filter(is.na(First.Lay.Date) & Fledge.Date.Estimated == 0)
    if (nrow(temp2) > 0) {

    temp2 <- temp2 %>% filter(!is.na(Fledge.Date)) %>%             # filter to fledge dates provided
      group_by(Attempt.ID) %>%                                     # group by attempt id
      summarise(Fledge = mean(Fledge.Date)) %>%                    # get the fledge date date and clutch size for each attempt
      mutate(Hatch = (Fledge - nestling))                          # hatch = fledge - avg nestling

    temp2 <- temp2 %>% select(Attempt.ID, Hatch)
    }


    ##################################################
    ##    Bind Data and update original df          ##
    ##################################################

    # Check if each temp was created
    df_list <- list()
    if (exists("temp")) df_list <- c(df_list, list(temp))
    if (exists("temp1")) df_list <- c(df_list, list(temp1))
    if (exists("temp2")) df_list <- c(df_list, list(temp2))

    # Bind any temp data into one df
    temp <- do.call(rbind, df_list)

    # Loop over each attempt to add data to the original df
    for (i in temp$Attempt.ID) {
      subset_data <- data[data$Attempt.ID == i, ]
      temp_row <- temp[temp$Attempt.ID == i, ]

      data$Hatch.Date[data$Attempt.ID == i] <- temp_row$Hatch
      data$Hatch.Date.Estimated[data$Attempt.ID == i] <- 1
    }


    # loop over each attempt to add data to the original df
    for (i in temp$Attempt.ID) {
      indicies <- which(data$Attempt.ID == i)                                   # get index in whole datatframe where Attempt is i
      data$Hatch.Date[indicies] <- as.Date(as.numeric(                          # move date from temp into whole dataframe's Hatch.Date
                                            temp[which(temp$Attempt.ID == i), 2]))
      data$Hatch.Date.Estimated[indicies] <- 1                                  # add 1 to that Attempt.ID's Lay Estimation column
    }
    } # end if subset > 0
    } # end if species > 0

  } #end loop over s in species

  ##################################
  ####        Output data       ####
  ##################################

  # Prep for and Export resulting dataframe
  pos <- 1
  envir = as.environment(pos)

  if (is.null(output)) {
    estimated.hatch <- NULL
    assign("estimated.hatch", data, envir = envir)
  } else {
    assign(paste0(output), data, envir = envir)
  }

} # end func


