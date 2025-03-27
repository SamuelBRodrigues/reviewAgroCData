#' Extract Early Childhood Education Enrollment Rate Data for Target Cities
#'
#' This function loads and processes data from an Excel file containing early childhood education
#' enrollment rates (taxa de matrícula em creches) for various municipalities, specifically for the year 2022.
#' The data is filtered to include only municipalities listed in the target cities data.
#'
#' @param file_path Character. Path to the Excel file containing the enrollment data. Default is
#'        'data_raw/Taxa líquida de matrículas em creches.xlsx'. The function will load the data from the file
#'        and filter for the year 2022, focusing on target cities.
#'
#' @return A data frame with the following columns:
#'         - `municipio_codigo`: The IBGE code of the municipality.
#'         - `municipio_nome`: The name of the municipality.
#'         - `taxa_de_matricula_2022`: The enrollment rate for the year 2022 in the target cities.
#'
#' @examples
#' \dontrun{
#' # Load and process data from the default file
#' taxa_matricula_data <- extract_taxa_matricula_creche()
#'
#' # Load and process data from a custom file path
#' taxa_matricula_data <- extract_taxa_matricula_creche("custom_data_raw/Taxa líquida de matrículas em creches.xlsx")
#' }
#'
#' @export
extract_taxa_matricula_creche <- function(
    file_path = 'data_raw/Taxa líquida de matrículas em creches.xlsx'
) {
  # Start message for file loading
  message('Loading data from file: ', file_path)

  # Read data, skipping the first 8 rows to avoid unnecessary headers
  taxa_matricula <- read_xlsx(
    file_path,
    col_names = TRUE,
    sheet = '1 -   Proporção de crianças ent',
    skip = 2,
    na = "-"
  )
  message('File loaded successfully.')

  # Load target cities data from the package
  message('Loading target cities data...')
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))
  message('Target cities data loaded.')

  # Filter and select relevant columns from INEP data
  message('Filtering and selecting relevant columns...')
  taxa_matricula_2022 <- taxa_matricula %>%
    dplyr::select(`Código IBGE`,
                  `Localidade`,
                  `​2022`) %>%
    dplyr::rename(`municipio_codigo` = `Código IBGE`,
                  `municipio_nome` =  Localidade,
                  `taxa_de_matricula_2022` = `​2022`)

  # Filter distortion rate data to keep only rows for target cities
  message('Filtering data for target cities...')
  taxa_matricula_2022 <- taxa_matricula_2022 %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Return the filtered data
  message('Data filtered and processed successfully.')
  return(taxa_matricula_2022)
}
