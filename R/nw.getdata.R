#' @title Download NestWatch Data
#'
#' @description Download archived versions of the NestWatch database, with optional control over what version is downloaded.
#'
#' @param version numeric; optional number corresponding to the desired dataset version. Defaults to latest available version
#'
#' @return two dataframes; `NW.attempts` containing nest-level parameters for each "Attempt.ID" and `NW.checks` containing check-level information for "Attempt.ID"s that have nest check data.
#' @export
#'
#' @details The NestWatch dataset consists of two large files and this function might ake severla minutes to run depending on yoru connection speed. Presently, there is no option to subset the data prior to download.
#'
#' @import reticulate
#' @importFrom utils read.csv
#' @importFrom utils unzip
#' @examples
#'\dontrun{
#' # Download version 1 of the data
#' nw.getdata(version = 1)
#'
#' # Download most recent version
#' nw.getdata()
#' }
nw.getdata <- function(version = NULL) {                                        # define getdata function, with one optional argument to change data version

  ### 1. Define version of dataset to be downloaded and prep for download
  if (is.null(version)) {
    DOI <- "10.17632/wjf794z7gc"
  } else {
    DOI <- paste("10.17632/wjf794z7gc", version, sep = ".")                   # add version to base DOI
  }

  dir.create("data-raw/temp-dir")                                             # create temporary directory for data to download to

  ### 2. Download the data using Reticulate and DataHugger
  reticulate::use_virtualenv()                                                # use a virtual instance of Python
  reticulate::virtualenv_install(packages = "datahugger", pip_options = character())  # virtually install DataHugger to retrieve Mendeley Data
  datahugger <- reticulate::import("datahugger")
  datahugger$get(DOI, unzip = T, "data-raw/temp-dir")

  ### 3. Unzip datafiles, add to global environment, remove temp directory
  files <- list.files("data-raw/temp-dir")                                             # get list of files in the temporary directory
  for (i in files) {                                                          # loop over files to unzip and store as .csv
    unzip(paste("data-raw/temp-dir", i, sep = "/"), exdir = "data-raw/temp-dir")
  }

  message("... NestWatch data files are being unzipped and extracted to your Global Enviorment. This may take a minute ...")

  ### 4. Prep for and output
  pos <- 1
  envir = as.environment(pos)

  files <- list.files("data-raw/temp-dir", pattern = "\\.csv$", full.names = TRUE)     # get list of .csv files
  NW.attempts <- read.csv(grep("attempt", files, value = TRUE))              # filter to get Attempt/Location dataset and move to Global Environment
  NW.checks   <- read.csv(grep("check",   files, value = TRUE))              # filter to get Nest Visits dataset and move to Global Environment
  assign("NW.attempts", NW.attempts, envir = envir)
  assign("NW.attempts", NW.attempts, envir = envir)

  unlink("data-raw/temp-dir", recursive = TRUE)                              # remove the temporary directory
}