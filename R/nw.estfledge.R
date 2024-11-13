#' Estimate Fledge Date for NestWatch Data
#'
#' @description Fledge Date data may be important for investigating relationships between nesting success and environmental variables. These data
#' in the NestWatch dataset are provided by participants, and the input of these dates may be overlooked by the participants. If not explicitly
#' provided by the participant, fledge date may be estimated based off hatch date or first lay date. This function checks, in order, if the nest
#' fledged successfully, and if a Hatch.Date was recorded or then if a First.Lay.Date was recorded. If these requirements are met, the function
#' uses user-input nest phenology time frames to estimate fledge date. Estimated dates are denoted with a \code{1} in \code{data$Fledge.Date.Estimated}.
#'
#' @details Precision of the fledge date estimates depend on what data is used to estimate the values from (and if those values are estimated). Users
#' should consider if they want to estimate fledge dates based on estimated hatch or lay dates. The function estimates fledge date by counting
#' forward from the hatch date (if available) the average number of nestling days (provided by the user). If hatch date is not available, the
#' function estimates fledge date based on first lay date, countersign forward the number of egg-laying-days based on clutch size (if available,
#' or user-provided average clutch size) and eggs-per-day of the species. #'
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
#' # All fledge dates if nest fledged (Outcome = s1) should be 2024-06-03
#' # Notem Attemp.ID "4" failed to fledged (outcome = "f"), so a fledge date is not estimated
#' data <- data.frame(Attempt.ID = c("1", "2", "3", "4"),
#'                    Species.Code = rep("carwre", 4),
#'                    First.Lay.Date = as.Date(c("2024-05-01", "2024-05-02",
#'                                               "2042-05-01", "2024-05-01")),
#'                    Hatch.Date = as.Date(c("2024-05-21", NA, NA, "2024-05-21")),
#'                    Fledge.Date = as.Date(c(NA, NA, NA, NA)),
#'                    Fledge.Date.Estimated = c(NA, NA, NA, NA),
#'                    Clutch.Size = c(NA, 3, 4, NA),
#'                    Young.Total = c(4, 3, 4, 4),
#'                    Outcome = c("s1", "s1", "s1", "f"))
#'
#' # Create phenology dataframe
#' phenology <- data.frame(Species = c("carwre"),
#'                         Clutch.Size  = c(4),
#'                         Eggs.per.Day = c(1),
#'                         Incubation   = c(16),
#'                         Nestling     = c(13),
#'                         Total = c(40))
#'
#' nw.estfledge(data = data, phenology = phenology)
nw.estfledge <- function(data, phenology, output = NULL) {


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
  Fledge.Date <- First.Lay.Date <- Hatch.Date <- Outcome <- Fledge <- First.Lay <- NULL


  ###########################
  ####  Function         ####
  ###########################

  # Force species column name to lowercase if upper
  if ("Species" %in% colnames(phenology)) {
    colnames(phenology)[colnames(phenology) == "Species"] <- "species"
  }

  message("... Estimating fledge dates, this may take some time ...")


  for (s in phenology$species) {

    # Define phenology vector elements by searching for column names
    spp_phen <- phenology %>% filter(species == s)
    clutch.size <- spp_phen[[grep("clutch.size", colnames(spp_phen), ignore.case = TRUE)]]
    eggs.per.day <- spp_phen[[grep("eggs.per.day", colnames(spp_phen), ignore.case = TRUE)]]
    inc <- spp_phen[[grep("incubation", colnames(spp_phen), ignore.case = TRUE)]]
    nestling <- spp_phen[[grep("nestling", colnames(spp_phen), ignore.case = TRUE)]]
    total <- spp_phen[[grep("total", colnames(spp_phen), ignore.case = TRUE)]]


    # Filter to a single species with no fledge date data
    sp_data <- data %>% filter(Species.Code == s & is.na(Fledge.Date))
    # If this subset contains data, continue (there may not be these cases), if not skip and display message
    if (nrow(sp_data) == 0) {message(paste0("Output Note: Either no Attempts exist for species", shQuote(s, type = "cmd"), ", or all Attempts already contain values for 'Fledge.Date'."))}
    if (nrow(sp_data) > 0) {

    # Subset to Attempts that fledged
    subset <- sp_data %>% filter(Outcome == "s1")
    # If this subset contains data, continue (there may not be these cases), if not skip and display message
    if (nrow(subset) == 0) {message(paste0("Output Note: No Fledge.Dates were estimated for species", shQuote(s, type = "cmd"), " as no attempts were successful."))}
    if (nrow(subset) > 0) {

    ########################################
    ##    No Fledge Date, Yes Hatch Date  ##
    ########################################

    # Filter to those attempts with hatch dates
    temp <- subset %>% filter(!is.na(Hatch.Date))
    if (nrow(temp) > 0) {                                          # If this subset contains data - continue, if not skip
    temp <- temp %>% group_by(Attempt.ID) %>%                      # group by attempt id
      summarise(Hatch.Date = max(Hatch.Date)) %>%                  # use max() as hack to get hatch date
      mutate(Fledge = (Hatch.Date + nestling))                     # fledge = hatch + avg nestling

    temp <- temp %>% select(Attempt.ID, Fledge)
    } # end if temp > 0

    ##################################################
    ##    No Fledge Date/Hatch Date, Yes First Lay  ##
    ##################################################

    # Filter to other attempts with first lay

    # Clutch size known
    temp1 <- subset %>% filter(is.na(Hatch.Date) & !is.na(First.Lay.Date) & Clutch.Size > 0)
    if(nrow(temp1) > 0) {                                          # If this subset contains data - continue, if not skip
    temp1 <- temp1 %>% group_by(Attempt.ID) %>%                    # group by attempt id
      summarise(First.Lay = max(First.Lay.Date),                   # use max() to get clutch and first lay
                Clutch.Size = max(Clutch.Size)) %>%
      mutate(Fledge = (First.Lay +                                 # Fledge = lay + clutch size * egg/day + avg inc + avg nestling
                         Clutch.Size * eggs.per.day + inc + nestling))
    temp1 <- temp1 %>% select(Attempt.ID, Fledge)
    } # end if temp1 > 0

    # Clutch size is not known
    temp2 <- subset %>% filter(is.na(Hatch.Date) & !is.na(First.Lay.Date) & is.na(Clutch.Size))
    if (nrow(temp2) > 0) {                                         # If this subset contains data - continue, if not skip
    temp2 <- temp2 %>% group_by(Attempt.ID) %>%                    # group by attempt id
      summarise(First.Lay = max(First.Lay.Date)) %>%               # use max() to get clutch and first lay
      mutate(Fledge = (First.Lay +                                 # Fledge = lay + avg clutch * egg/day + avg inc + avg nestling
                         (clutch.size * eggs.per.day) + inc + nestling))
    temp2 <- temp2 %>% select(Attempt.ID, Fledge)
    } # end if temp2 > 0


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

      data$Fledge.Date[data$Attempt.ID == i] <- temp_row$Fledge
      data$Fledge.Date.Estimated[data$Attempt.ID == i] <- 1
    }

    } # end of if species subset has any successful nests
    } # end of if there are NAs in Fledge.Date for a species
  } #end of species loop

  ##################################
  ####        Output data       ####
  ##################################

  # Prep for and Export resulting dataframe
  pos <- 1
  envir = as.environment(pos)

  if (is.null(output)) {
    estimated.fledge <- NULL
    assign("estimated.fledge", data, envir = envir)
  } else {
    assign(paste0(output), data, envir = envir)
  }

} # end function
