#' Estimate First Lay Date for NestWatch Data
#'
#' @description First Lay Date data is important for investigating changes in nesting phenology over time. These data in the NestWatch dataset are provided by
#' participants, and the input of these dates may be overlooked by participants. If not explicitly provided by the participant, first lay date may be estimated
#' based off Hatch Date, Fledge Date, or if the nest was visited while eggs were still being laid. This function checks, in order, if the nest was visited
#' during egg laying, has a hatch date, or has a fledge date. If so, it uses user-input nest phenology timeframes to estimate the first lay date. Estimated
#' dates are denoted with a \code{1} in \code{data$First.Lay.Date.Estimated}.
#'
#' @details Precision of the first lay date estimates depend on what data is used to estimate the values from. The function estimates based off of egg counts
#' observed during lay first, followed by hatch date, and then fledge date. First lay dates are more likely to have high precision is based on known egg dates,
#' as there is less variability in eggs laid per day than number of incubation days or number of days until fledge (which can both vary by individual,
#' latitude, climate, and other factors).
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
#'     \item \code{Total.Nesting.Period}: Average number of days between first lay date and fledge date.
#'     }
#' @param output character; An optional character string to custom name the output dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples
#' # Simplified NestWatch data with missing data
#' # All hatch dates should be 2024-05-01
#' data <- data.frame(Attempt.ID = c(1, 2, 3, 3),
#'   Species.Code = rep("carwre", 4),
#'   Visited.During.Egg.Laying = c(0, 0, NA, NA),
#'   First.Lay.Date = as.Date(rep(NA, 4)),
#'   Hatch.Date = as.Date(c("2024-05-20", NA, NA, NA)),
#'   Fledge.Date = as.Date(c(NA, "2024-06-02", NA, NA)),
#'   Clutch.Size = c(4, 4, NA, NA),
#'   Visit.Datetime = as.Date(c("2024-05-19", "2024-06-01", "2024-05-02", "2024-05-15")),
#'   Host.Eggs.Count = c(0, 0, 2, 4))
#'
#' # Create phenology dataframe
#' phen <- data.frame(Species = c("bewwre", "carwre"),
#'                    Clutch.Size  = c(5, 4),
#'                    Eggs.per.Day = c(1, 1),
#'                    Incubation   = c(16, 16),
#'                    Nestling     = c(16, 13),
#'                    Total = c(50, 40))
#'
#' # Run function
#' nw.estfirstlay(data = data, phenology = phen)

nw.estfirstlay <- function(data, phenology, output = NULL) {

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
  Species <- Species.Code <- Attempt.ID <- Visit.Datetime <- Host.Eggs.Count <- Visited.During.Egg.Laying <- Eggs.per.Day <- NULL
  Incubation <- Netsling <- Clutch.Size <- Hatch.Date <- Fledge.Date <- Species <- Days.of.Lay <- First.Lay.Date <- NULL
  Host.Eggs.Increases <- LayVisit <- eggs_on_date <- NULL



  ###########################
  ####  Function         ####
  ###########################

  # Force species column name to lowercase if upper
  if ("Species" %in% colnames(phenology)) {
    colnames(phenology)[colnames(phenology) == "Species"] <- "species"
  }

  # For each species provided, try to estimate First Lay Date
  for (s in phenology$species) {

    # Define phenology vector elements by searching for column names
    spp_phen <- phenology %>% filter(species == s)
    clutch.size <- spp_phen[[grep("clutch.size", colnames(spp_phen), ignore.case = TRUE)]]
    eggs.per.day <- spp_phen[[grep("eggs.per.day", colnames(spp_phen), ignore.case = TRUE)]]
    inc <- spp_phen[[grep("incubation", colnames(spp_phen), ignore.case = TRUE)]]
    nestling <- spp_phen[[grep("nestling", colnames(spp_phen), ignore.case = TRUE)]]
    total <- spp_phen[[grep("total", colnames(spp_phen), ignore.case = TRUE)]]


    # Filter to a single species
    sp_data <- data %>% filter(Species.Code == s & is.na(First.Lay.Date))
    if (nrow(sp_data) == 0) {message(paste0("Output Note: Either no Attempts exist for species ", shQuote(s, type = "cmd"), ", or all Attempts already contain values for 'First.Lay.Date'."))}
    if (nrow(sp_data) > 0) {

      # Determine if nests were visited during egg lay (and was not marked as such)
      temp <- sp_data %>%
        arrange(Attempt.ID, Visit.Datetime) %>%                                # Sort the dataframe by Attempt.ID and Visit.Datetime
        group_by(Attempt.ID) %>%
        mutate(Host.Eggs.Increases = c(FALSE, diff(Host.Eggs.Count) > 0)) %>%  # for each timestep, did eggs increase (ie. visited during lay)
        summarise(LayVisit = any(Host.Eggs.Increases)) %>%                     # for each attempt, was it visited during lay
        filter(LayVisit == T)

      if (nrow(temp) > 0){
        # Update sp_data$Visited.During.Egg.Laying (not raw data)
        matching_ids <- sp_data$Attempt.ID %in% temp$Attempt.ID
        sp_data$Visited.During.Egg.Laying[matching_ids] <- ifelse(is.na(sp_data$Visited.During.Egg.Laying[matching_ids]) |  # if visited is NA OR
                                                                    sp_data$Visited.During.Egg.Laying[matching_ids] == 0,   # if visited is 0
                                                                  1,                                                        # then update with 1
                                                                  sp_data$Visited.During.Egg.Laying[matching_ids])          # if not, keep original
      } # end denoting if visited during lay but not marked as such


      ########################################
      ##    No Lay Date, Visited in Lay     ##
      ########################################

      # Subset to Attempts visited during Lay but no lay date
      subset <- sp_data %>% filter(Visited.During.Egg.Laying == 1 & is.na(First.Lay.Date))

      if (nrow(subset) > 0){
        # Make new df to hold egg dates
        eggdates <- subset %>% filter(!is.na(Visit.Datetime)) %>%
          filter(Host.Eggs.Count > 0) %>%                                             # filter just visits that recorded eggs
          group_by(Attempt.ID)                                                        # we will look by Attempt.ID
        # Check to see if there are nests in this subset, if so continue, if not skip
        if (nrow(eggdates) > 0) {
          eggdates <- eggdates %>% summarise(Visit.Datetime = as.Date(min(Visit.Datetime, na.rm = T)),  # calculate the min date eggs were recorded
                    eggs_on_date = first(Host.Eggs.Count))                            # find who many eggs were on that day
          # Estimate when First Lay date was
          eggdates <- eggdates %>% mutate(Days.of.Lay = eggs_on_date * eggs.per.day, # using the earliest # eggs in nest, how many days has it been in lay
                                        First.Lay.Date = Visit.Datetime - Days.of.Lay + 1) # calculate how many days of lay there have been, and back est first lay date
          temp <- eggdates %>% select(Attempt.ID, First.Lay.Date)                       # simplified df

          # loop over each attempt to add data to the original df
          for (i in temp$Attempt.ID) {
            indicies <- which(data$Attempt.ID == i)                                       # get index in whole datatframe where Attempt is i
            data$First.Lay.Date[indicies] <- as.Date(as.numeric(temp[which(temp$Attempt.ID == i), 2]))  # move date from temp into whole dataframe's First.Lay.Date
            data$First.Lay.Date.Estimated[indicies] <- 1                                  # add 1 to that Attempt.ID's Lay Estimation column
          } # end loop
        } # end if there is data to calc egg dates off of



      } # end subset for if no lay date but visited during lay


      ########################################
      ##    No Lay Date, Yes Hatch Date     ##
      ########################################

      # Subset to Attempts without first lay date to those not visited during egg laying, but have hatch dates
      #    These hatch dates can be known or estimated. So this section's estimates are less certain than using egg dates
      subset <- sp_data %>% filter((Visited.During.Egg.Laying == 0 & is.na(First.Lay.Date))) %>%
        filter(!is.na(Hatch.Date))

      # If subset has identified data, continue
      if(nrow(subset) > 0){
        subset <- subset %>% group_by(Attempt.ID) %>%                    # make small dataframe, max() used but only one value exists for each
          summarise(Clutch.Size = mean(Clutch.Size),
                    Hatch.Date = mean(Hatch.Date),
                    First.Lay.Date = mean(First.Lay.Date))

        # If clutch size data present:
        #   Count back provided incubation days, count back # eggs*egg/d in the clutch +1
        temp <- subset %>% filter(Clutch.Size > 0)
        temp <- temp %>% mutate(First.Lay.Date = (Hatch.Date - inc - (Clutch.Size * eggs.per.day) + 1))

        # If clutch size is NA or 0:
        #   Count back provided avg clutch size*egg/d +1
        temp1 <- subset %>% filter(!(Clutch.Size > 0) | is.na(Clutch.Size))
        temp1 <- temp1 %>% mutate(First.Lay.Date = (Hatch.Date - inc - (clutch.size * eggs.per.day) + 1))

        # Bind temp and temp1
        temp <- rbind(temp, temp1)

        # loop over each attempt to add data to the original df
        for (i in temp$Attempt.ID) {
          indicies <- which(data$Attempt.ID == i)                                       # get index in whole datatframe where Attempt is i
          data$First.Lay.Date[indicies] <- as.Date(as.numeric(temp[which(temp$Attempt.ID == i), 4]))  # move date from temp into whole dataframe's First.Lay.Date
          data$First.Lay.Date.Estimated[indicies] <- 1                                  # add 1 to that Attempt.ID's Lay Estimation column
        }
      } # end No Lay Date, Yes Hatch Date subset


      #############################################
      ##    No Lay, No Hatch, Yes Fledge Date    ##
      #############################################

      # Subset to Attempts without lay ro natch dates, but have fledge dates
      #    These fledge dates can be known or estimated. So this section's estimates are less certain than using hatch dates, and much more uncertain than using egg dates
      subset <- sp_data %>% filter(Visited.During.Egg.Laying == 0) %>%
        filter(is.na(First.Lay.Date)) %>%
        filter(is.na(Hatch.Date)) %>%
        filter(!is.na(Fledge.Date))

      # If subset has identified data, continue
      if(nrow(subset) > 0){
        subset <- subset %>% group_by(Attempt.ID) %>%                    # make small dataframe, mean() hack used but only one value exists for each
          summarise(Clutch.Size = mean(Clutch.Size),
                    Fledge.Date = mean(Fledge.Date),
                    First.Lay.Date = mean(First.Lay.Date))

        # If clutch size data present:
        #   Count back provided incubation days, nestling days, # eggs in the clutch*egg/day +1
        temp <- subset %>% filter(Clutch.Size > 0)
        temp <- temp %>% mutate(First.Lay.Date = (Fledge.Date - nestling - inc - (Clutch.Size * eggs.per.day) + 1))

        # If clutch size is NA or 0:
        #   Count back provided avg clutch size +1
        temp1 <- subset %>% filter(!(Clutch.Size > 0) | is.na(Clutch.Size))
        temp1 <- temp1 %>% mutate(First.Lay.Date = (Fledge.Date - inc - (clutch.size * eggs.per.day) + 1))

        # Bind temp and temp1
        temp <- rbind(temp, temp1)
        # loop over each attempt to add data to the original df
        for (i in temp$Attempt.ID) {
          indicies <- which(data$Attempt.ID == i)                                       # get index in whole datatframe where Attempt is i
          data$First.Lay.Date[indicies] <- as.Date(as.numeric(temp[which(temp$Attempt.ID == i), 4]))  # move date from temp into whole dataframe's First.Lay.Date
          data$First.Lay.Date.Estimated[indicies] <- 1                                  # add 1 to that Attempt.ID's Lay Estimation column
        }
      } # end  No Lay, No Hatch, Yes Fledge Date subset

    } # end if the species has any NA lay dates
  } # end loop over each species



  ##################################
  ####        Output data       ####
  ##################################

  # Prep for and Export resulting dataframe
  pos <- 1
  envir = as.environment(pos)

  if (is.null(output)) {
    estimated.data <- NULL
    assign("estimated.lay", data, envir = envir)
  } else {
    assign(paste0(output), data, envir = envir)
  }


} #end func

