#' Download and Extract School Dropout Data
#'
#' This function downloads and extracts school dropout data from the INEP database.
#'
#' @param dir_path A character string specifying the directory path where the file will be downloaded and extracted. Default is 'data/'.
#'
#' @details
#' The function ensures the target directory exists, downloads the ZIP file containing school dropout data, extracts its contents, and removes the ZIP file after extraction.
#'
#' @examples
#'
#' download_abandono_escolar()
#'
#' @export

download_abandono_escolar <- function(dir_path = "data/") {
  url <- "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2021/tx_rend_municipios_2021.zip"

  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Download the file
  file_path <- .download_file(url, dir_path)

  # Extract the ZIP file
  .extract_zip(file_path, dir_path)

  # Remove the ZIP file after extraction
  .remove_zip(file_path)
}
