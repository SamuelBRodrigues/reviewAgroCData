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
    file_path = 'data_raw/atlasivs_dadosbrutos_pt_v2.xlsx',
    ano = 2022
) {
  message('Loading file...')
  # Read data, keeping headers
  ivs_data <- readxl::read_xlsx(
    file_path,
    col_names = TRUE
  )
  ano = 2022
  ano <- ano |> as.character()

  # Filter and select data
  ivs <- ivs_data |>
    dplyr::filter(
      nivel == "regiao,uf,rm,municipio",
      label_cor == "Total Cor",
      label_sexo == "Total Sexo",
      label_sit_dom == "Total Situação de Domicílio"
    ) |>
    dplyr::select(
      municipio,
      ano,
      ivs,
      ivs_infraestrutura_urbana,
      ivs_capital_humano,
      ivs_renda_e_trabalho
    ) |>
    dplyr::rename(
      "municipio_codigo" = municipio
    ) |>
    dplyr::filter(
      ano == ano
    )

  return(ivs)
}
