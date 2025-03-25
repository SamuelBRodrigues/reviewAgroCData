#' Download and Extract Distortion Rate Data
#'
#' This function downloads the distortion rate dataset from the INEP website, extracts it, and removes the ZIP file after extraction.
#'
#' @param dir_path Character. The directory where the file will be downloaded and extracted. Default is `"data/"`.
#'
#' @return No return value. The function downloads and extracts the dataset into the specified directory.
#'
#' @examples
#' \dontrun{
#' download_taxa_de_distorcao()
#' download_taxa_de_distorcao("custom_directory/")
#' }
#'
#' @export
download_taxa_de_distorcao <- function(dir_path = "data/") {
  url <- "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2022/TDI_2022_MUNICIPIOS.zip"

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
