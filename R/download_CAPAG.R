#' Download CAPAG (Municipal Government Financial Management) Data
#'
#' This function downloads the CAPAG (Government Financial Management Indicator) data for municipalities
#' from the Tesouro Transparente platform. The data is provided in an Excel file and will be saved to the
#' specified directory.
#'
#' @param dir_path Character. Directory path where the downloaded file will be saved. Default is "data/".
#'        The directory will be created if it doesn't already exist.
#'
#' @return Invisible path to the downloaded file (string). The file will be named
#'         "CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx" and saved in the specified directory.
#'
#' @examples
#' \dontrun{
#' # Download the CAPAG data to the default "data/" directory
#' download_CAPAG()
#'
#' # Download the CAPAG data to a custom directory
#' download_CAPAG("my_data/")
#' }
#'
#' @export
download_CAPAG <- function(dir_path = "data/") {
  url <- "https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/6a218451-f1b4-4fce-ac2a-00a3675bf4eb/download/CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx"

  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Download the file
  file_path <- .download_file(url, dir_path)
}
