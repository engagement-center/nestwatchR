% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nw.getdata.R
\name{nw.getdata}
\alias{nw.getdata}
\title{Download NestWatch Data}
\usage{
nw.getdata(version = NULL)
}
\arguments{
\item{version}{numeric; optional number corresponding to the desired dataset version. Defaults to latest available version}
}
\value{
two dataframes; `NW.attempts` containing nest-level parameters for each "Attempt.ID" and `NW.checks` containing check-level information for "Attempt.ID"s that have nest check data.
}
\description{
Download archived versions of the NestWatch database, with optional control over what version is downloaded.
}
\details{
The NestWatch dataset consists of two large files and this function might take several minutes to run depending on your connection speed. Presently, there is no option to subset the data prior to download.
}
\examples{
\dontrun{
# Download version 1 of the data
nw.getdata(version = 1)

# Download most recent version
nw.getdata()
}
}
