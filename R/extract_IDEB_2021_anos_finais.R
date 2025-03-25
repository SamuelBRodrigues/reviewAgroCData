#' Extract IDEB 2021 Data for Target Cities
#'
#' This function loads the IDEB data for the year 2021 from a given Excel file, filters it
#' to include only the target cities specified in the `target_cities.rda` file, and returns
#' the filtered data with relevant columns.
#'
#' @param file_path A character string indicating the path to the Excel file containing the IDEB
#' data for 2021. The default path is 'data/divulgacao_anos_finais_municipios_2023/divulgacao_anos_finais_municipios_2023.xlsx'.
#'
#' @return A data frame containing the filtered IDEB data for the target cities, with the following columns:
#' \itemize{
#'   \item municipio_codigo: The municipality code.
#'   \item municipio_nome: The name of the municipality.
#'   \item estado_sigla: The abbreviation of the state.
#'   \item IDEB_2021: The IDEB value for the year 2021.
#' }
#'
#' @examples
#' # Load IDEB data for target cities
#' ideb_data <- extract_IDEB_2021_anos_finais()
#'
#' @export
extract_IDEB_2021_anos_finais <- function(
    file_path = 'data/divulgacao_anos_finais_municipios_2023/divulgacao_anos_finais_municipios_2023.xlsx'
) {
  # Start message for file loading
  message('Loading IDEB data from file: ', file_path)

  # Read data, skipping the first 9 rows to avoid unnecessary headers
  ideb_data <- read_xlsx(
    file_path,
    col_names = TRUE,
    skip = 9
  )
  message('File loaded successfully.')

  # Load target cities data from the package
  message('Loading target cities data...')
  load(system.file("data", "target_cities.rda", package = "getAgroCData"))
  message('Target cities data loaded.')

  # Filter and select relevant columns from IDEB data
  message('Filtering and selecting relevant columns...')
  ideb_2021 <- ideb_data %>%
    dplyr::select(CO_MUNICIPIO,
                  NO_MUNICIPIO,
                  SG_UF,
                  VL_OBSERVADO_2021) %>%
    dplyr::rename(municipio_codigo = CO_MUNICIPIO,
                  municipio_nome =  NO_MUNICIPIO,
                  estado_sigla = SG_UF,
                  IDEB_2021 = VL_OBSERVADO_2021)

  # Filter IDEB data to keep only rows for target cities
  message('Filtering data for target cities...')
  ideb_2021 <- ideb_2021 %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Return the filtered IDEB data
  message('Data filtered and processed successfully.')
  return(ideb_2021)
}
