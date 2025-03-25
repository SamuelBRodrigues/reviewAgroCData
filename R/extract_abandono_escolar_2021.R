#' Extract School Dropout Rates for 2021
#'
#' This function loads and processes school dropout rate data from an INEP dataset.
#' It filters the dataset to include only relevant municipalities and selects specific columns.
#'
#' @param file_path Character. Path to the Excel file containing the dropout rate data.
#' Default is 'data/tx_rend_municipios_2021/tx_rend_municipios_2021.xlsx'.
#'
#' @return A data frame containing school dropout rates for selected municipalities,
#' with columns for municipality code, municipality name, state abbreviation,
#' fundamental education dropout rate, and high school dropout rate.
#'
#' @examples
#'
#' # Using the default file path
#' df <- extract_abandono_escolar_2021()
#'
#' # Specifying a different file path
#' df <- extract_abandono_escolar_2021("path/to/your/file.xlsx")
#'
#' @export
extract_abandono_escolar_2021 <- function(
    file_path = 'data/tx_rend_municipios_2021/tx_rend_municipios_2021.xlsx'
) {
  # Start message for file loading
  message('Loading INEP data from file: ', file_path)

  # Read data, skipping the first 8 rows to avoid unnecessary headers
  inep_data <- read_xlsx(
    file_path,
    col_names = TRUE,
    skip = 8,
    na = "Fonte: Censo da Educação Básica 2021/INEP."
  )
  message('File loaded successfully.')

  # Load target cities data from the package
  message('Loading target cities data...')
  load(system.file("data", "target_cities.rda", package = "getAgroCData"))
  message('Target cities data loaded.')

  # Filter and select relevant columns from INEP data
  message('Filtering and selecting relevant columns...')
  abandono_escolar_2021 <- inep_data %>%
    dplyr::filter(NO_CATEGORIA == "Total" & NO_DEPENDENCIA == "Total") %>%
    dplyr::select(CO_MUNICIPIO,
                  NO_MUNICIPIO,
                  SG_UF,
                  `3_CAT_FUN`,
                  `3_CAT_MED`) %>%
    dplyr::rename(`municipio_codigo` = CO_MUNICIPIO,
                  `municipio_nome` =  NO_MUNICIPIO,
                  `estado_sigla` = SG_UF,
                  `taxa_abandono_ensino_fundamental` = `3_CAT_FUN`,
                  `taxa_abadono_ensino_medio` = `3_CAT_MED`)

  # Filter IDEB data to keep only rows for target cities
  message('Filtering data for target cities...')
  abandono_escolar_2021 <- abandono_escolar_2021 %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Return the filtered IDEB data
  message('Data filtered and processed successfully.')
  return(abandono_escolar_2021)
}
