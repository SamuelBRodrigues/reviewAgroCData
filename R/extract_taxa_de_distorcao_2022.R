#' Extract and Filter 2022 Distortion Rate Data
#'
#' This function loads the 2022 distortion rate dataset from an Excel file, filters the relevant data, and returns only the records for target cities.
#'
#' @param file_path Character. The path to the Excel file containing the 2022 distortion rate data. Default is `"data_raw/TDI_2022_MUNICIPIOS/TDI_MUNICIPIOS_2022.xlsx"`.
#'
#' @return A data frame containing the filtered distortion rate data for target cities.
#'
#' @examples
#' \dontrun{
#' extract_taxa_de_distorcao_2022()
#' extract_taxa_de_distorcao_2022("custom_path/TDI_2022.xlsx")
#' }
#'
#' @export
extract_taxa_de_distorcao_2022 <- function(
    file_path = 'data_raw/TDI_2022_MUNICIPIOS/TDI_MUNICIPIOS_2022.xlsx'
) {
  # Start message for file loading
  message('Loading INEP data from file: ', file_path)

  # Read data, skipping the first 8 rows to avoid unnecessary headers
  inep_data <- read_xlsx(
    file_path,
    col_names = TRUE,
    skip = 7,
    na = "Fonte: Censo da Educação Básica 2022/INEP."
  )
  message('File loaded successfully.')

  # Load target cities data from the package
  message('Loading target cities data...')
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))
  message('Target cities data loaded.')

  # Filter and select relevant columns from INEP data
  message('Filtering and selecting relevant columns...')
  taxa_de_distorcao_2022 <- inep_data %>%
    dplyr::filter(NO_CATEGORIA == "Total" & NO_DEPENDENCIA == "Total") %>%
    dplyr::select(CO_MUNICIPIO,
                  NO_MUNICIPIO,
                  SG_UF,
                  `FUN_CAT_0`,
                  `MED_CAT_0`) %>%
    dplyr::rename(`municipio_codigo` = CO_MUNICIPIO,
                  `municipio_nome` =  NO_MUNICIPIO,
                  `estado_sigla` = SG_UF,
                  `taxa_de_distorcao_ensino_fundamental` = `FUN_CAT_0`,
                  `taxa_de_distorcao_ensino_medio` = `MED_CAT_0`)

  # Filter distortion rate data to keep only rows for target cities
  message('Filtering data for target cities...')
  taxa_de_distorcao_2022 <- taxa_de_distorcao_2022 %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Return the filtered data
  message('Data filtered and processed successfully.')
  return(taxa_de_distorcao_2022)
}
