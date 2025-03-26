#' Extract and Process CAPAG Current Savings Data for 2022
#'
#' This function extracts and processes the current savings data (poupança corrente) for municipalities from the
#' CAPAG (Government Financial Management Indicator) dataset for the year 2022. The data is filtered to include
#' only the target cities and relevant columns are selected.
#'
#' @param file_path Character. The path to the CAPAG data Excel file.
#'        The default is 'data/CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx'.
#'
#' @return A data frame containing the current savings data (`poupança corrente`) for the target cities.
#'         The resulting data frame includes the following columns:
#'         \describe{
#'           \item{municipio_codigo}{Municipality code (IBGE code).}
#'           \item{municipio_nome}{Municipality name.}
#'           \item{estado_sigla}{State abbreviation.}
#'           \item{poupanca_corrente}{Current savings indicator for 2022.}
#'         }
#'
#' @details
#' This function reads the CAPAG data, loads the target cities data, and filters the CAPAG data to keep only the
#' relevant columns and rows for the target cities. The result is a data frame with current savings information
#' (poupança corrente) for 2022.
#'
#' @examples
#' \dontrun{
#' # Extract and process the CAPAG current savings data for 2022
#' poupanca_corrente_2022_data <- extract_CAPAG_poupanca_corrente_2022()
#' }
#'
#' @export
extract_CAPAG_poupanca_corrente_2022 <- function(
    file_path = 'data/CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx'
) {
  # Start message for file loading
  message('Loading CAPAG data from file: ', file_path)

  # Read data
  capag_data <- read_xlsx(
    file_path,
    col_names = TRUE,
  )
  message('File loaded successfully.')

  # Load target cities data from the package
  message('Loading target cities data...')
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))
  message('Target cities data loaded.')

  # Filter and select relevant columns from CAPAG data
  message('Filtering and selecting relevant columns...')
  poupanca_corrente_2022 <- capag_data %>%
    dplyr::select(`Cod.IBGE`,
                  Município,
                  UF,
                  Indicador_2_Revisão) %>%
    dplyr::rename(`municipio_codigo` = Cod.IBGE,
                  `municipio_nome` =  Município,
                  `estado_sigla` = UF,
                  `poupanca_corrente` = `Indicador_2_Revisão`)

  # Filter CAPAG data to keep only rows for target cities
  message('Filtering data for target cities...')
  poupanca_corrente_2022 <- poupanca_corrente_2022 %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Return the filtered current savings data
  message('Data filtered and processed successfully.')
  return(poupanca_corrente_2022)
}
