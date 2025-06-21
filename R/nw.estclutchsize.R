#' Estimate Missing Summary Clutch Size Values in NestWatch Data
#'
#' @description Clutch size is an important value for estimating nest dates and determining the successfulness of a nest.
#' NestWatch participants may forget to enter clutch size into the nest attempt's summary data. But clutch size information may be estimated
#' from the data of individual nest visits. This function used available data from any entered nest visits to estimate maximum host egg count
#' (clutch size).
#'
#' @details This function looks for data contained in several columns to estimate what the clutch size of the nest was. Because NestWatch
#' participants do not necessarily visit each nest everyday or record all data fields on every visit the data needed to estimate clutch size
#' may be spread over many columns. Depending on the data available, the certainty of the clutch size estimate may vary, or in some cases,
#' may not be estimable. For instance, a successful nest with 4 eggs recorded on several checks and 4 fledged young observed would have high
#' certainty that the clutch size was 4. However, if a nest was observed with 2 eggs on one check and a week later was found predated, the
#' certainty of the clutch size is not known. In this case the clutch size would be estimated as 2 but may have been higher depending on the
#' exact (unobserved) failure date.
#'
#'
#' @param data dataframe; A dataframe containing merged NestWatch attempts and visits data.
#' @param output character; An optional character vector to rename the resulting output.
#'
#' @return dataframe; A dataframe with an additional binary column \code{Clutch.Size.Estimated} where \code{1} denotes the clutch size was
#' estimated by the function and \code{0} denotes the clutch size was provided by the participant.
#' @export
#'
#' @examples
#' # Example simplified NestWatch dataset:
#' data <- data.frame(Attempt.ID = c(1, 2, 3, 4, 5, 6),
#'                    Visit.ID = c(1, 2, 3, 4, 5, 6),
#'                    Clutch.Size = c(1, NA, NA, NA, NA, NA),
#'                    Host.Eggs.Count = c(1, 2,  NA, NA, NA, 3),
#'                    Young.Total = c(1, 2, 3, 3, 4, NA),
#'                    Live.Host.Young.Count = c(1, 1, 2, 3, 1, 1),
#'                    Unhatched.Eggs = c(0, 0, NA, NA, 1, 3),
#'                    Young.Fledged = c(1, 0, NA, NA, 1, 3),
#'                    Dead.Host.Young.Count = c(0, NA, NA, 1, 0,1))
#'
#' # Estimate Clutch Sizes if NA
#' nw.estclutchsize(data = data, output = "out")
#'
#' out$Clutch.Size   # clutch size values
#' out$Clutch.Size.Estimated   # binary indicator if clutch size is estimated (1) or not (0)
nw.estclutchsize <- function(data, output = NULL){

  ###########################
  ####  Check arguments  ####
  ###########################

  # Stop function if output was not named AND no assignment arrow was used
  if ((interactive() && sys.nframe() == 1) & is.null(output)) {
    stop("Please either assign the output to an object or specify an output name using output = 'object_name'.")
  }

  # Check the dataframe is merged NW data
  if (missing(data)){
    stop("Argument 'data' must be a dataframe of merged NestWatch attempts and visits data.")
  }
  if (all(!(c("Species.Code", "Visit.ID") %in% names(data)))){
    stop("Argument 'data' must be a dataframe of merged NestWatch attempts and visits data.")
  }
  # Check output is character vector
  if(!is.null(output) & !inherits(output, "character"))
    stop("Argument 'output' must be a character vector.")

  ###########################
  ####       Setup       ####
  ###########################

  # Initiate Column Names
  Clutch.Size <- Attempt.ID <- Host.Eggs.Count <- Unhatched.Eggs <- Young.Total <- Live.Host.Young.Count <- NA
  Dead.Host.Young.Count <- Young.Fledged <- Clutch.Size.Estimated <- Clutch.Size.x <- Clutch.Size.y <- NA
  Young.in.Nest <- est1 <- est2 <- est3 <- envir <- NA

  # If clutch size is reported give estimated column a 0
  data <- data %>% mutate(Clutch.Size.Estimated = ifelse(!is.na(Clutch.Size), 0, NA)) %>%
    relocate(Clutch.Size.Estimated, .after = Clutch.Size)
  df <- data



  ##################################
  ####   Estimate Clutch Size   ####
  ##################################


  # For blank clutches, estimate clutch size from checks data
  # Some prep, remove all NA rows
  # Get the sum each check of Live + Dead host young. If both are NA, sum = NA. If one is NA sum is the other
  df <- df %>% filter(is.na(Clutch.Size))

  if (nrow(df) == 0) {message(paste0("Output Note: All attempts already contain values for 'Clutch.Size'."))}
  if (nrow(df) > 0) {

    # By visit get sum of live and dead young
    df$Young.in.Nest <- apply(df[, c("Live.Host.Young.Count", "Dead.Host.Young.Count")], 1, function(row) {
      if (all(is.na(row))) {
        return(NA)
      } else {
        return(sum(row, na.rm = TRUE))
      }
    })


    # Change all NAs to "-999"
    cols_to_replace <- names(df)
    df[cols_to_replace] <- lapply(df[cols_to_replace], function(x) ifelse(is.na(x), -999, x))

    # Summarize by attempt: (1) max egg count, (2) numb unhatched eggs, (3) max young in the nest (dead + alive), (4) numb fledges
    df <- df %>% group_by(Attempt.ID) %>%
      summarise(Host.Eggs.Count = max(Host.Eggs.Count),
                Unhatched.Eggs = unique(Unhatched.Eggs),
                Young.Total = max(Young.Total, Young.in.Nest),
                Young.Fledged = unique(Young.Fledged))

    # Remove attempts that have all NA (-999) values, we can not estimate these
    df <- df[rowSums(df[ , -1]) != (-999*4), ]


    # Find the max nest contents between (1) number of observed host eggs, (2) young (alive/dead) + unhatched eggs, (3) fledged + unhatched eggs
    df$est1 <- df$Host.Eggs.Count
    df$est2 <- apply(df[, c("Young.Total", "Unhatched.Eggs")], 1, function(row) {   # sum = NA if all NA
      if (all(row == -999)) {                                  # if both are NA return NA
        return(-999)
      } else if ((sum(row) > (-999*2)) & (sum(row) < 0)){      # if one is NA, return the value
        return(sum(row) - -999)
      } else {                                                 # if both have values, return sum
        return(sum(row))
      }
    })

    df$est3 <- apply(df[, c("Young.Fledged", "Unhatched.Eggs")], 1, function(row) {   # sum = NA if all NA
      if (all(row == -999)) {                                  # if both are NA return NA
        return(-999)
      } else if ((sum(row) > (-999*2)) & (sum(row) < 0)){      # if one is NA, return the value
        return(sum(row) - -999)
      } else {                                                 # if both have values, return sum
        return(sum(row))
      }
    })

    df <- df %>% mutate(Clutch.Size = pmax(est1, est2, est3))
    df <- df %>% select(c("Attempt.ID", "Clutch.Size"))



    ##################################
    ####   Update Field in data   ####
    ##################################

    # Replace NA clutches with data if we were able to estimate it, cleanup columns
    out <- data %>% left_join(df, by = c("Attempt.ID")) %>%
      mutate(Clutch.Size = coalesce(Clutch.Size.x, Clutch.Size.y)) %>%
      select(-Clutch.Size.x,-Clutch.Size.y) %>%
      relocate(Clutch.Size, .after = Young.Fledged) %>%
      relocate(Clutch.Size.Estimated, .after = Clutch.Size)

    # Add `1` to Clutch.Size.Estimated if value was estimated
    estIDs <- unique(df$Attempt.ID)
    out$Clutch.Size.Estimated <- ifelse(out$Attempt.ID %in% estIDs, 1, out$Clutch.Size.Estimated)

    # Change -999 back to NA
    cols_to_replace <- names(out)

    out[cols_to_replace] <- lapply(out[cols_to_replace], function(x) {  # to preserve datetime formatting
      if (is.numeric(x)) {
        replace(x, x == -999, NA)
      } else if (inherits(x, "Date") || inherits(x, "POSIXct")) {
        x  # Leave date columns as they are
      } else {
        x  # Leave other columns as they are
      }
    })




  } # end if df has any NA clutch sizes



  ##################################
  ####        Output data       ####
  ##################################

  # Prep for and Export resulting dataframe
  pos <- 1
  envir = as.environment(pos)



  # Export resulting dataframe
  if (is.null(output)) {
    filtered.data <- NULL
    assign("filtered.data", out, envir = envir)
  } else {
    assign(paste0(output), out, envir = envir)
  }


} # end of function


