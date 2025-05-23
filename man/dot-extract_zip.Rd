% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.extract_zip}
\alias{.extract_zip}
\title{Extract a ZIP file to a specified directory}
\usage{
.extract_zip(file_path, dir_path = "data_raw/")
}
\arguments{
\item{file_path}{Character. The path to the ZIP file to be extracted.}

\item{dir_path}{Character. The directory where the contents should be extracted. Default is "data_raw/".}
}
\value{
NULL. The function extracts the files.
}
\description{
Extract a ZIP file to a specified directory
}
\examples{
.extract_zip("data_raw/data.zip")
}
