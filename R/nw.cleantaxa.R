#' Perform Cleaning Actions on Species Names & Codes
#'
#' @description This function can optionally remove attempts for species identified as hybrids, "spuhs", or "slashes" and  preform
#' a "taxonomic roll up" of subspecies to the species level.
#'
#' @details The NestWatch platform uses eBird taxonomy to help users auto-complete species names. This allows users to select any
#' available eBird taxonomic unit, including spuhs (ie. "chickadee sp."), slashes (ie. "Tree/Violet-green Swallow"),
#' and subspecies/forms (ie."House Wren (northern)"). In some cases, the user may decide to omit spuhs and slashes because species
#' is not known with certainty. Or the user may want to analyze all subspecies/forms of a species together if these differentiations
#' are not biologically relevant to the analysis. This function will update both the Common Name and Species Code of the attempt
#' according to the current version of eBird taxonomy in \code{\link[auk:ebird_taxonomy]{auk::ebird_taxonomy}}.
#'
#' @seealso \code{\link[auk:ebird_taxonomy]{auk::ebird_taxonomy}}
#'
#' @param data dataframe; A dataframe containing NestWatch data.
#' @param spuh logical; Should attempt with "sp." be removed?
#'  \itemize{
#'   \item if \code{NULL}, the default, retains identified rows in the output dataframe.
#'   \item if \code{TRUE}, removes identified rows from the output dataframe.
#'   \item if \code{FALSE}, retains identified rows in the output dataframe.
#' }
#'
#' @param slash logical; Should attempt with "/" be removed?
#'  \itemize{
#'   \item if \code{NULL}, the default, retains identified rows in the output dataframe.
#'   \item if \code{TRUE}, removes identified rows from the output dataframe.
#'   \item if \code{FALSE}, retains identified rows in the output dataframe.
#' }
#' @param hybrid logical; Should attempt with "(hybrid)" be removed?
#'  \itemize{
#'   \item if \code{NULL}, the default, retains identified rows in the output dataframe.
#'   \item if \code{TRUE}, removes identified rows from the output dataframe.
#'   \item if \code{FALSE}, retains identified rows in the output dataframe.
#' }
#' @param rollsubspecies logical; Should attempt with Species.Name of the form "species (subspecies/form) be rolled up to the species level?
#' Uses the current eBird taxonomy as in \code{\link[auk:ebird_taxonomy]{auk::ebird_taxonomy}}.
#'  \itemize{
#'   \item if \code{NULL}, the default, retains original Species.Names and Species.Codes.
#'   \item if \code{TRUE}, preforms taxonomic roll up for all attempts with subspecies/form designation and updates Species.Code.
#'   \item if \code{FALSE}, retains original Species.Names and Species.Codes.
#' }
#' @param output character; An optional character string to custom name the output dataframe
#'
#' @return a dataframe
#' @import auk
#' @export
#'
#' @examples
#' # Data with species name variations
#' df <- data.frame(Attempt.ID = c(1, 1, 2, 3, 4),
#'                  Species.Name = c("House Wren (northern)", "House Wren (northern)",
#'                                  "chickadee sp.", "Tree/Violet-green Swallow",
#'                                  "Mallard x American Black Duck (hybrid)"),
#'                 Species.Code = c("houwre1", "houwre1", "chicka1", "y00701", "x00004"))
#'
#' # Remove just ".sp" attempts
#' nw.cleantaxa(df, spuh = TRUE)
#'
#' # Remove "/" and hybrid attempts, roll up subspecies to species
#' nw.cleantaxa(df, slash = TRUE, hybrid = TRUE, rollsubspecies = TRUE)
nw.cleantaxa <- function(data, spuh = FALSE, slash = FALSE, hybrid = FALSE, rollsubspecies = FALSE, output = NULL){

  # Remove "spuhs" from the data
  if(isTRUE(spuh)){
    data <- data[!grepl("sp.", data$Species.Name), ]
  }
  # Remove "/" species
  if(isTRUE(slash)){
    data <- data[!grepl("/", data$Species.Name), ]
  }
  # Remove hybrids
  if(isTRUE(hybrid)){
    data <- data[!grepl("\\bhybrid\\b", data$Species.Name), ]
  }


  # Roll subspecies "species (subspecies)" to full species
  # Ignore hybrids
  if(isTRUE(rollsubspecies)){
    # Get eBird taxonomy from auk, rename columns
    taxa <- auk::ebird_taxonomy
    names(taxa)[names(taxa) == "common_name"] <- "Species.Name"
    names(taxa)[names(taxa) == "species_code"] <- "Species.Code"

    # Get row numbers of subsps
    rows <- grep("^(?!.*hybrid).*\\(", data$Species.Name, perl = TRUE)
    # Strip subsp info if not hybird
    data$Species.Name <- ifelse(grepl("\\bhybrid\\b", data$Species.Name),
                                data$Species.Name,
                                sub(" \\(.*", "", data$Species.Name))
    # Temp df to store codes and join on common name
    new_codes <- data[rows, c("Attempt.ID", "Species.Name", "Species.Code")]
    new_codes <- left_join(new_codes, taxa[, c("Species.Code", "Species.Name")],     # get df of old/new codes for rows
                           by = "Species.Name", relationship = "many-to-one")
    # Update data with corrected sp codes
    data[rows, "Species.Code"] <- new_codes$Species.Code.y
    }


  # Prep for and Export resulting dataframe
  pos <- 1
  envir = as.environment(pos)
  if (is.null(output)) {
    cleaned.data <- NULL
    assign("cleaned.data", data, envir = envir)
  } else {
    assign(paste0(output), data, envir = envir)
  }

}
