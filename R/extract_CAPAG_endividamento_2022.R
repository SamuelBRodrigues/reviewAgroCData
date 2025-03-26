#' Extract and Process CAPAG Debt Data for 2022
#'
#' This function extracts and processes the debt information (endividamento) from the CAPAG (Government Financial
#' Management Indicator) data for municipalities, specifically for the year 2022. The data is filtered to include
#' only target cities and relevant columns.
#'
#' @param file_path Character. The file path of the CAPAG data Excel file.
#'        The default is 'data/CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx'.
#'
#' @return A data frame containing the debt information (`endividamento`) for the target cities, including
#'         columns: `municipio_codigo`, `municipio_nome`, `estado_sigla`, and `endividamento`.
#'
#' @details
#' This function reads the CAPAG data, loads target cities data, and filters the CAPAG data to keep only the relevant
#' columns and rows for the specified target cities. The final result is a data frame with the debt information for 2022.
#'
#' @examples
#' \dontrun{
#' # Extract and process the CAPAG debt data for 2022
#' endividamento_2022_data <- extract_CAPAG_endividamento_2022()
#' }
#'
#' @export
extract_CAPAG_endividamento_2022 <- function(
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
  load(system.file("data", "target_cities.rda", package = "getAgroCData"))
  message('Target cities data loaded.')

  # Filter and select relevant columns from CAPAG data
  message('Filtering and selecting relevant columns...')
  endividamento_2022 <- capag_data %>%
    dplyr::select(`Cod.IBGE`,
                  Município,
                  UF,
                  Indicador_1_Revisão) %>%
    dplyr::rename(`municipio_codigo` = Cod.IBGE,
                  `municipio_nome` =  Município,
                  `estado_sigla` = UF,
                  `endividamento` = `Indicador_1_Revisão`)

  # Filter CAPAG data to keep only rows for target cities
  message('Filtering data for target cities...')
  endividamento_2022 <- endividamento_2022 %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Return the filtered debt data
  message('Data filtered and processed successfully.')
  return(endividamento_2022)
}
