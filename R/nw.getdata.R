#' @title Download NestWatch Data
#'
#' @description Download archived versions of the NestWatch database, with optional control over what version is downloaded.
#'
#' @param version numeric; optional number corresponding to the desired dataset version. Defaults to latest available version
#'
#' @return two dataframes; `NW.attempts` containing nest-level parameters for each "Attempt.ID" and `NW.checks` containing check-level information for "Attempt.ID"s that have nest check data.
#' @export
#'
#' @details The NestWatch dataset consists of two large files and this function might take several minutes to run depending on your connection speed. Presently, there is no option to subset the data prior to download.
#'
#' @importFrom readr read_csv
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr http_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils download.file
#'
#' @examples
#'\dontrun{
#' # Download version 1 of the data
#' nw.getdata(version = 1)
#'
#' # Download most recent version
#' nw.getdata()
#' }
nw.getdata <- function(version = NULL) {

  ### 1. Define DOI and download location
  doi <- "wjf794z7gc"
  output_folder <- "temp-dir"


  ### 2. Call to Mendeley API for file information
  base_api_url <- "https://data.mendeley.com/public-api/datasets/"  # Mendeley API call

  # Get dataset versions
  versions_url <- paste0(base_api_url, doi, "/versions")
  versions_response <- GET(versions_url)
  versions <- fromJSON(content(versions_response, "text"))

  # Get a particular data version
  v <- version
  if(!is.null(version)){      # if version is provided
    v <- v
  } else {v <- max(as.numeric(versions$version))}  # if not get the latest version

  # Get dataset files
  files_url <- paste0(base_api_url, doi, "/files?folder_id=root&version=", v)
  files_response <- GET(files_url)

  if (http_status(files_response)$category != "Success") {
    stop("Failed to retrieve dataset files.")
  }

  files <- fromJSON(content(files_response, "text"))

  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }




  ### 3. Download the files
  for(i in 1:nrow(files)){
    file_url  <- files$content_details$download_url[i]
    file_dest <- file.path(output_folder, paste0(sub("\\.csv.zip$", "", files$filename[i]), ".csv"))

    cat("Downloading:", files$filename[i], "\n")

    download.file(file_url, file_dest, mode = "wb")
  }



  ### 4. Prep for and output
  pos <- 1
  envir = as.environment(pos)

  files <- list.files("temp-dir", pattern = "\\.csv$", full.names = TRUE)    # get list of .csv files
  NW.attempts <- suppressMessages(readr::read_csv(grep("attempt", files, value = TRUE)))     # filter to get Attempt/Location dataset and move to Global Environment
  NW.checks   <- suppressMessages(readr::read_csv(grep("check",   files, value = TRUE)))    # filter to get Nest Visits dataset and move to Global Environment

  # Replace spaces in column names with "."
  names(NW.attempts) <- gsub(" ", ".", names(NW.attempts))
  names(NW.checks) <- gsub(" ", ".", names(NW.checks))

  # Assign
  assign("NW.attempts", NW.attempts, envir = envir)
  assign("NW.checks", NW.checks, envir = envir)

  unlink("temp-dir", recursive = TRUE)                              # remove the temporary directory

  message(paste0("NestWatch Open Dataset (v", v, ") have been loaded to your global environment."))



} # end get data function
