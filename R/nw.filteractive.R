#' Filter Nesting Attempts to the Active Nesting Period
#'
#' @description Trimming nesting attempt checks to the active periods (nest contains or likely contains host or brood parasite eggs and/or chicks) may
#' provide better pictures of nest start and end dates. Knowing nest active dates can be useful when calculating nest exposure days as well as
#' comparing changes in the nesting period over space and time. This function notes individual nest checks within an attempt if no
#' eggs or chicks of either host or parasite species are recorded (in numbers, as present, or likely present (i.e. an adult is on the nest
#' and was not flushed)). This differs slightly from \link[nestwatchR]{nw.filterphenology}'s \code{trim_to_active = T} argument where build
#' is considered active in the whole nest phenologic cycle and automatically included the fail/fledge check where the first 0 eggs/young are recorded after
#' being positive. See details below.
#'
#'
#' @param data dataframe; A dataframe of merged NestWatch attempt and visits data. Date columns must have class \code{Date}
#' and Datetime columns must have class \code{POSIXct}.
#' @param mode \code{"flag"} or \code{"remove"}; A character string defining if the analyst wants the identified nesting attempts to be
#' flagged with "FLAGGED" in a new column, or removed from the dataset.
#' @param include_ffcheck logical; TRUE or FALSE indicating whether the fledge/fail check (first 0 eggs/young recorded after last positive count)
#' should be retained (not flagged or removed). Though the nest is inactive, retaining this may allow more estimation of fledge/fail dates
#' closer to the true, unobserved date. Defaults set to FALSE.
#' @param output character; An optional character string to custom name the output dataframe.
#'
#' @details This function summarizes by nest check, if eggs or chicks were counted or present, or if there is evidence of adult behavior indicative of
#' eggs or chicks being present but not visible in the nest. If evidence of eggs or chicks are not present, the check is noted as not active.
#' This function then flags (or removes) the leading and trailing checks noted as inactive. If a check is noted as inactive, but occur between
#' two active checks, that check will not be flagged -- this usually occurs when a participant has not recorded full data for that check, like
#' in cases where an adult was present but eggs/chicks were not looked for. This method flags (or removes) the last "fail/fledge check", as the contents
#' of the nest in that case are usually 0. This function also does not account for nests being failed by abandonment or partial predation, as there
#' would still be a record of eggs being present despite the nest having failed (knowingly or unknowingly by the participant). This function contrasts with
#' \link[nestwatchR]{nw.filterphenology}'s \code{trim_to_active = T} argument, where in the latter only host eggs/young are considered,
#' the "fail/fledge" check (first 0 egg/chick count after a positive count) is retained automatically, checks prior to eggs/young are retained if a
#' nest is incomplete or complete (suspected to be in build), and a species-specific phenology table is used to identify period of unacceptable length (such as long in incubation may suggest abandonment).
#'
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
#' # Simplified NestWatch dataset with nest summary dates
#' # Checks 1 & 5 should be flagged as inactive (no eggs/young/or adults on the nest)
#'data <- data.frame(Attempt.ID = c("A1", "A1", "A1", "A1", "A1"),
#'                   Species.Code = rep("carwre", 5),
#'                   # simulate data for four visits to one nest
#'                   Visit.Datetime = c(as.POSIXct(rep(NA, 5))),
#'                   Visit.ID = c("S001", "S002", "S003", "S004", "S005"),
#'                   Visit.Datetime = as.POSIXct(c("2024-05-01", "2024-05-05",
#'                                                 "2024-05-08", "2024-05-15", "2024-05-18")),
#'                   Adult.Activity = c(NA, NA, NA, NA, NA),
#'                   Host.Eggs.Count = c(0, 2, 0, 2, 0),
#'                   Host.Eggs.Present.Uncounted = rep(NA, 5),
#'                   Brood.Parasite.Eggs.Count = c(0, 0, NA, 0, 0),
#'                   Brood.Parasite.Eggs.Present.Uncounted = c(0, 0, NA, 0, 0),
#'                   Live.Host.Young.Present.Uncounted = rep(NA, 5),
#'                   Live.Host.Young.Count = c(0, 0, NA, 0, 0),
#'                   Brood.Parasite.Live.Young.Count = c(0, 0, NA, 0, 0),
#'                   Brood.Parasite.Live.Young.Present.Uncounted = c(0, 0, NA, 0, 0),
#'                   Nest.Status = rep(NA, 5))
#'
#' nw.filteractive(data = data, mode = "flag")
nw.filteractive <- function(data, mode, include_ffcheck = FALSE, output = NULL) {

  #####################################
  ###   Function Parameters Check   ###
  #####################################

  # stop if dataframe is not provided
  if(missing(data)){
    stop("Argument 'data' must be a dataframe of merged NestWatch attempts and visits data.")
  }
  # Stop if dataframe is not a NW merged dataframe
  if (all(!(c("Species.Code", "Visit.ID") %in% names(data)))){
    stop("Argument 'data' must be a dataframe of merged NestWatch attempts and visits data.")
  }
  # Stops Function if 'mode' is not specified.
  if (missing(mode)) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  # Stops Function if 'mode' is invalid.
  if (!mode %in% c("flag", "remove")) {
    stop("Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  }
  if(!is.logical(include_ffcheck)) {
      stop("Invalid 'include_ffcheck'. Value must be logical T or F.")
  }
  # Stops function if date-time is incorrectly classed.
  if (!all(sapply(data$Visit.Datetime, function(x) inherits(x, c("POSIXct", "POSIXt"))))) {
    stop("Column 'Visit.Datetime' must be of class 'POSIXct' or 'POSIXt'.")
  }



  #####################################
  ###            Setup              ###
  #####################################

  # Initiate column names
  Flagged.Check <- Active.Check <- n_checks <- Attempt.ID <- Visit.Datetime <-  Live.Host.Young.Count <- NA
  Host.Eggs.Count <- Host.Eggs.Present.Uncounted <- Brood.Parasite.Eggs.Count <- Brood.Parasite.Eggs.Present.Uncounted <- NA
  Live.Host.Young.Count <- Live.Host.Young.Present.Uncounted <- Brood.Parasite.Live.Young.Count <- NA
  Brood.Parasite.Live.Young.Present.Uncounted <- Adult.Activity <- ff_check <- .is_active <- .within_active_period <- NA

  # Prep dataframe
  data <- data %>% group_by(Attempt.ID) %>% arrange(Visit.Datetime, .by_group = T)  # Group data by attempt and sort by visit datetime

  # Mark groups if they only have a single line of data (these checks are not relevant here)
  data <- data %>% mutate(n_checks = n())
  hold <- data %>% filter(n_checks == 1)  # break out and hold single line rows, bring back at end
  data <- data %>% filter(n_checks != 1)


    #####################################
    ###     Note inactive checks      ###
    #####################################

    # If nest check has eggs or live young (including adults on but not flushed), mark as active check
    data <- data %>%
      mutate(
      Active.Check = case_when(
        # If ALL egg/young/adult status columns are NA, return NA. This would likely be a historic attempt w/o checks
        if_all(
          c(Host.Eggs.Count,
            Host.Eggs.Present.Uncounted,
            Brood.Parasite.Eggs.Count,
            Brood.Parasite.Eggs.Present.Uncounted,
            Live.Host.Young.Count,
            Live.Host.Young.Present.Uncounted,
            Brood.Parasite.Live.Young.Count,
            Brood.Parasite.Live.Young.Present.Uncounted,
            Adult.Activity), is.na) ~ NA_real_,

        # If ANY of these columns indicate activity, return 1
        if_any(c(Host.Eggs.Count,
                 Brood.Parasite.Eggs.Count,
                 Live.Host.Young.Count,
                 Brood.Parasite.Live.Young.Count), ~ dplyr::coalesce(.x >= 1, FALSE)) |
          if_any(c(Host.Eggs.Present.Uncounted,
                   Brood.Parasite.Eggs.Present.Uncounted,
                   Live.Host.Young.Present.Uncounted,
                   Brood.Parasite.Live.Young.Present.Uncounted), ~ dplyr::coalesce(.x == 1, FALSE)) |
          if_any(Adult.Activity, ~ dplyr::coalesce(.x == "ra", FALSE)) ~ 1,

        # Otherwise, return 0
        TRUE ~ 0))

    ######################################################
    ###     Flag leading/trailing inactive checks      ###
    ######################################################

    # We only want to flag/remove leading (build, pre-lay) and trailing (post fail/fledge) checks
    # If a check is inactive in between, we want to keep that. Likely when data is incompletely filled out.
    # We will grab the fledge/fail check after for if include_ffcheck = T
    data <- data %>%
      # Group by attempt and arrange by visit datetime
      group_by(Attempt.ID) %>%
      arrange(Visit.Datetime, .by_group = TRUE) %>%
      mutate(
        # TRUE only where Active.Check is explicitly 1
        .is_active = coalesce(Active.Check == 1, FALSE),

        # TRUE from the first active check through the last active check
        .within_active_period = cumany(.is_active) & rev(cumany(rev(.is_active))),

        # Flag known-inactive rows outside the active period
        Flagged.Check = case_when(
          coalesce(Active.Check == 0, FALSE) & !.within_active_period ~ "FLAGGED",
          TRUE ~ NA_character_
        )
      )

    # Remove flag on FF check IF include_ffcheck = T
    if (isTRUE(include_ffcheck)) {
      data <- data %>% mutate(ff_check = as.integer(Flagged.Check == "FLAGGED" &
                                                    lag(is.na(Flagged.Check), default = FALSE)))  # mark first inactive check from end
      data$Flagged.Check[data$ff_check == 1] <- NA   # if that got marked as the ff check, unflag it
      data <- data %>% select(-ff_check)
    }


  # Remove intermediate columns before bringing back held data
  data <- data %>% select(-.is_active, -.within_active_period, -Active.Check)

  # Bind back in single line attempts
  data <- data %>% bind_rows(hold)

  # Remove remaining temp columns, relocate columns, and %>%
  data <- data %>% select(-n_checks) %>%
                   relocate(Flagged.Check, .before = Attempt.ID) %>%
                   arrange(Attempt.ID, Visit.Datetime) %>% ungroup()



  #####################################
  ###        Mode == "flag"         ###
  #####################################

  # If mode == "flag"
  if (mode == "flag") {
    # Export resulting dataframe
    # If `output` is mentioned
    if (!is.null(output)) {
      if (!is.character(output) || length(output) != 1) {
        stop("The 'output' argument must be a single character string.")
      }

      # Assign the dataframe to the user's calling environment (parent.frame())
      assign(output, data, envir = parent.frame())
      message("... Identified nesting attempts have been noted with 'FLAGGED' in the in column 'Flagged.Check'.")


      # Return invisibly so it doesn't double-print to the console
      return(invisible(data))
    }

    message("... Identified nesting attempts have been noted with 'FLAGGED' in the in column 'Flagged.Check'.")
    # Standard  return if output = NULL
    data


  } else {

    #####################################
    ###       Mode == "remove"        ###
    #####################################

    # if mode was remove
    # Filter out any nest checks that were flagged and remove column used to flag
    data <- data[!grepl("FLAGGED", data$Flagged.Check), ]
    data <- data %>% select(-Flagged.Check)

    # Export resulting dataframe
    # If `output` is mentioned
    if (!is.null(output)) {
      if (!is.character(output) || length(output) != 1) {
        stop("The 'output' argument must be a single character string.")
      }

      # Assign the dataframe to the user's calling environment (parent.frame())
      assign(output, data, envir = parent.frame())

      # Return invisibly so it doesn't double-print to the console
      message("... Identified nesting attempts have been removed from the new dataset.")
      return(invisible(data))
    }

    message("... Identified nesting attempts have been removed from the new dataset.")
    # Standard  return if output = NULL
    data
  }


} # end of function
