#' Extract and Filter IVS 2010 Data for Target Cities
#'
#' This function extracts data from a given Excel file, filters it for the year 2020,
#' and selects data for cities that are in the predefined target list. The selected data includes
#' the municipality name, state name, and two IVS indicators: income and work, and urban infrastructure.
#'
#' @param file_path A character string specifying the path to the Excel file containing the IVS data.
#' Defaults to `'data_raw/atlasivs_dadosbrutos_pt_v2.xlsx'`.
#'
#' @details
#' The function reads an Excel file containing the IVS data using the `readxl` package. It then loads a list
#' of target cities from a predefined `.rda` file located within the `getAgroCData` package. The data is filtered
#' for the year 2020 and for cities that match those in the target cities list. The selected columns include:
#' the municipality name, state name, and two IVS indicators: income and work (`ivs_renda_e_trabalho`),
#' and urban infrastructure (`ivs_infraestrutura_urbana`).
#'
#' @return A filtered data frame containing the selected information for the target cities for the year 2020.
#' The returned data includes:
#' - `nome_municipio`: The name of the municipality.
#' - `nome_uf`: The name of the state.
#' - `ivs_renda_e_trabalho`: The income and work IVS indicator.
#' - `ivs_infraestrutura_urbana`: The urban infrastructure IVS indicator.
#'
#' @examples
#' # Extract and filter the IVS 2010 data for target cities
#' ivs_2010_data <- extract_IVS_2010()
#'
#' # Specify a custom file path for the IVS data
#' ivs_2010_data_custom <- extract_IVS_2010(file_path = "custom/path/to/file.xlsx")
#'
#' @export
#' @importFrom readxl read_xlsx
#' @import dplyr
extract_IVS_2010 <- function(
    file_path = 'data_raw/atlasivs_dadosbrutos_pt_v2.xlsx'
) {
  message('Loading file...')
  # Read data, keeping headers
  ivs_data <- readxl::read_xlsx(
    file_path,
    col_names = TRUE
  )

  # Load target cities data from package
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))

  message('Applying filters...')

  # Filter and select data
  ivs_2010 <- ivs_data %>%
    dplyr::filter(ano == 2020) %>%
    dplyr::select(nome_municipio,
                  nome_uf,
                  ivs_renda_e_trabalho,
                  ivs_infraestrutura_urbana)

  # Display message
  message("IVS 2010 data for target cities successfully extracted and filtered.")

  message('Complete')

  return(ivs_2010)
}
