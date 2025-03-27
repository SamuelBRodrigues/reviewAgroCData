#' Download IDEB (Índice de Desenvolvimento da Educação Básica) Data for Fundamental Education
#'
#' This function downloads two ZIP files containing IDEB data for municipalities in Brazil
#' for the year 2023: one for the "Anos Iniciais" (Early Years) and another for the
#' "Anos Finais" (Final Years). The function downloads, extracts, and removes the ZIP files
#' after the extraction process is completed.
#'
#' @param dir_path Path to the directory where the files will be saved and extracted.
#' Defaults to `"data_raw/"`.
#'
#' @return Invisibly returns the paths of the downloaded and extracted files.
#'
#' @details
#' The function performs the following steps:
#' 1. Downloads two ZIP files containing IDEB data (one for Early Years and one for Final Years).
#' 2. Ensures that the specified directory exists and creates it if necessary.
#' 3. Extracts the contents of both ZIP files using the `.extract_zip` function.
#' 4. Removes the ZIP files after extraction using the `.remove_zip` function.
#'
#' The final extracted data will be saved in the specified directory, with the ZIP files
#' being removed after the process.
#'
#' @examples
#' \dontrun{
#' # Download IDEB data for Fundamental Education to the default directory ("data_raw/")
#' download_IDEB_fundamental()
#'
#' # Download IDEB data to a custom directory
#' download_IDEB_fundamental(dir_path = "data_raw/")
#' }
#'
#' @export
download_IDEB_fundamental <- function(dir_path = "data_raw/") {
  # Anos iniciais:
  url_f1 <- "https://download.inep.gov.br/ideb/resultados/divulgacao_anos_iniciais_municipios_2023.zip"
  # Anos finais:
  url_f2 <- "https://download.inep.gov.br/ideb/resultados/divulgacao_anos_finais_municipios_2023.zip"

  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Download the files
  file_path_f1 <- .download_file(url_f1, dir_path)
  file_path_f2 <- .download_file(url_f2, dir_path)

  # Extract the ZIP files
  .extract_zip(file_path_f1, dir_path)
  .extract_zip(file_path_f2, dir_path)

  # Remove the ZIP files after extraction
  .remove_zip(file_path_f1)
  .remove_zip(file_path_f2)
}
