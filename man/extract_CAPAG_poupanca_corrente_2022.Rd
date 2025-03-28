% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_CAPAG_poupanca_corrente_2022.R
\name{extract_CAPAG_poupanca_corrente_2022}
\alias{extract_CAPAG_poupanca_corrente_2022}
\title{Extract and Process CAPAG Current Savings Data for 2022}
\usage{
extract_CAPAG_poupanca_corrente_2022(
  file_path = "data_raw/CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx"
)
}
\arguments{
\item{file_path}{Character. The path to the CAPAG data Excel file.
The default is 'data_raw/CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx'.}
}
\value{
A data frame containing the current savings data (`poupança corrente`) for the target cities.
        The resulting data frame includes the following columns:
        \describe{
          \item{municipio_codigo}{Municipality code (IBGE code).}
          \item{municipio}{Municipality name.}
          \item{estado_sigla}{State abbreviation.}
          \item{poupanca_corrente}{Current savings indicator for 2022.}
        }
}
\description{
This function extracts and processes the current savings data (poupança corrente) for municipalities from the
CAPAG (Government Financial Management Indicator) dataset for the year 2022. The data is filtered to include
only the target cities and relevant columns are selected.
}
\details{
This function reads the CAPAG data, loads the target cities data, and filters the CAPAG data to keep only the
relevant columns and rows for the target cities. The result is a data frame with current savings information
(poupança corrente) for 2022.
}
\examples{
\dontrun{
# Extract and process the CAPAG current savings data for 2022
poupanca_corrente_2022_data <- extract_CAPAG_poupanca_corrente_2022()
}

}
