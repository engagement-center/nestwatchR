#' Perform Cleaning Actions on Species Names & Codes
#'
#' @description This function can optionally remove attempts for species identified as hybrids, "spuhs", or "slashes" and  perform
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
#' @param rm_spuh logical; Should attempt with "sp." be removed?
#'  \itemize{
#'   \item if \code{NULL}, the default, retains identified rows in the output dataframe.
#'   \item if \code{TRUE}, removes identified rows from the output dataframe.
#'   \item if \code{FALSE}, retains identified rows in the output dataframe.
#' }
#'
#' @param rm_slash logical; Should attempt with "/" be removed?
#'  \itemize{
#'   \item if \code{NULL}, the default, retains identified rows in the output dataframe.
#'   \item if \code{TRUE}, removes identified rows from the output dataframe.
#'   \item if \code{FALSE}, retains identified rows in the output dataframe.
#' }
#' @param rm_hybrid logical; Should attempt with "(hybrid)" be removed?
#'  \itemize{
#'   \item if \code{NULL}, the default, retains identified rows in the output dataframe.
#'   \item if \code{TRUE}, removes identified rows from the output dataframe.
#'   \item if \code{FALSE}, retains identified rows in the output dataframe.
#' }
#' @param roll_subspecies logical; Should attempt with Species.Name of the form "species (subspecies/form) be rolled up to the species level?
#' Uses the current eBird taxonomy as in \code{\link[auk:ebird_taxonomy]{auk::ebird_taxonomy}}.
#'  \itemize{
#'   \item if \code{NULL}, the default, retains original Species.Names and Species.Codes.
#'   \item if \code{TRUE}, performs taxonomic roll up for all attempts with subspecies/form designation and updates Species.Code.
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
#' nw.cleantaxa(df, rm_spuh = TRUE)
#'
#' # Remove "/" and hybrid attempts, roll up subspecies to species
#' nw.cleantaxa(df, rm_slash = TRUE, rm_hybrid = TRUE, roll_subspecies = TRUE)
nw.cleantaxa <- function(data, rm_spuh = FALSE, rm_slash = FALSE, rm_hybrid = FALSE, roll_subspecies = FALSE, output = NULL){

  # Initialize Column Names
  species_code <- category <- report_as <- Species.Code <- common_name <- NULL


  # Get eBird taxonomy, bind in category and report as
  taxa <- auk::ebird_taxonomy
  data <- data %>% left_join(taxa %>% select(species_code, category, report_as), by = c("Species.Code" = "species_code"))



  # Remove "spuhs" from the data
  if(isTRUE(rm_spuh)){
    data <- data %>% filter(category != "spuh")
  }
  # Remove "/" species
  if(isTRUE(rm_slash)){
    data <- data %>% filter(category != "slash")
  }
  # Remove hybrids/intergrades
  if(isTRUE(rm_hybrid)){
    data <- data %>% filter(category != "hybrid") %>% filter(category != "intergrade")
  }


  # Roll subspecies "species (subspecies)" to full species
  if(isTRUE(roll_subspecies)){
    # Change spp codes
    data <- data %>% mutate(Species.Code = if_else(is.na(report_as), Species.Code, report_as))
    # Change common name
    data <- data %>% left_join(taxa %>% select(species_code, common_name), by = c("Species.Code" = "species_code"))
    data <- data %>% mutate(Species.Name = common_name)
    data <- data %>% select(-common_name) # remove common name column
  }

  # Remove bound in columns
  data <- data %>% select(-category, -report_as)

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
