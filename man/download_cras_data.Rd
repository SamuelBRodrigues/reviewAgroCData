% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_cras_data.R
\name{download_cras_data}
\alias{download_cras_data}
\title{Download dos dados do SUAS - CRAS}
\usage{
download_cras_data(dir = "data_raw", ano = 2023)
}
\arguments{
\item{dir}{diretório onde os dados serão baixados}

\item{ano}{Ano de interesse dos dados}
}
\value{
Um arquivo compactado no diretorio indicado
}
\description{
A função baixa os dados do CRAS da bases do senso SUAS
}
\examples{
\dontrun{
  download_cras_data(dir = "data_raw", ano = "2022")
}
}
