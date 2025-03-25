#' Extract PIB per capita data for the year 2020
#'
#' This function reads municipal PIB (Produto Interno Bruto) data from an Excel file and returns filtered data for 2020,
#' including only the target cities specified in the package's internal data.
#'
#' @param file_path Path to the Excel file. Default is 'data/PIB dos Munic�pios - base de dados 2010-2021.xlsx'.
#' @return A tibble containing filtered 2020 data with columns: 'Código do Município', 'Sigla da Unidade da Federação',
#'         'Nome do Município', and 'Produto Interno Bruto per capita, \r\na preços correntes\r\n(R$ 1,00)'.
#' @export
#' @importFrom readxl read_xlsx
#' @import dplyr

extract_PIB_per_capita_2020 <- function(
    file_path = 'data/PIB dos Munic�pios - base de dados 2010-2021.xlsx'
) {
  # Read data, keeping headers
  pib_data <- readxl::read_xlsx(
    file_path,
    col_names = TRUE
  )

  # Load target cities data from package
  load(system.file("data", "target_cities.rda", package = "getAgroCData"))

  # Filter and select data
  pib_2020 <- pib_data %>%
    dplyr::filter(Ano == 2020,
                  `Código do Município` %in% target_cities$municipio_codigo) %>%
    dplyr::select(`Código do Município`,
                  `Sigla da Unidade da Federação`,
                  `Nome do Município`,
                  `Produto Interno Bruto per capita, \r\na preços correntes\r\n(R$ 1,00)`
    ) %>%

  return(pib_2020)
}
