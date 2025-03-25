#' Extract IDEB 2021 Data for Initial Years
#'
#' This function extracts and processes IDEB (Índice de Desenvolvimento da Educação Básica) data for the year 2021, focusing on public schools in target municipalities.
#'
#' @param file_path A character string specifying the path to the Excel file containing IDEB data. Default is 'data/divulgacao_anos_iniciais_municipios_2023/divulgacao_anos_iniciais_municipios_2023.xlsx'.
#'
#' @return A data frame containing filtered IDEB data with the following columns:
#'   - `municipio_codigo`: Municipality code
#'   - `municipio_nome`: Municipality name
#'   - `estado_sigla`: State abbreviation
#'   - `IDEB_2021`: IDEB score for 2021 (observed value)
#'
#' @details
#' The function reads the specified Excel file, processes it to keep only relevant columns, filters data for public schools, and further narrows it down to target municipalities loaded from the package dataset.
#'
#' @examples
#'
#'
#' ideb_data <- extract_IDEB_2021_anos_iniciais()
#' head(ideb_data)
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select rename filter
#' @export
extract_IDEB_2021_anos_iniciais <- function(
    file_path = 'data/divulgacao_anos_iniciais_municipios_2023/divulgacao_anos_iniciais_municipios_2023.xlsx'
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
                  REDE,
                  VL_OBSERVADO_2021) %>%
    dplyr::rename(`municipio_codigo` = CO_MUNICIPIO,
                  `municipio_nome` =  NO_MUNICIPIO,
                  `estado_sigla` = SG_UF,
                  `IDEB_2021` = VL_OBSERVADO_2021) %>%
    dplyr::filter(REDE == 'Pública')

  # Filter IDEB data to keep only rows for target cities
  message('Filtering data for target cities...')
  ideb_2021 <- ideb_2021 %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Return the filtered IDEB data
  message('Data filtered and processed successfully.')
  return(ideb_2021)
}
