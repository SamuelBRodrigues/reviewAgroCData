#' Download RAIS 2022 Data
#'
#' This function downloads the RAIS 2022 data file from the official government website
#' and saves it in the specified directory.
#'
#' @param dir_path A character string specifying the directory where the file will be saved. Default is "data/".
#'
#' @return No return value. The function downloads the file and saves it locally.
#'
#' @examples
#' download_RAIS("my_data/")
#' download_RAIS()
#'
#' @export

download_RAIS <- function(dir_path = "data/") {
  url_2022 <- "https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/estatisticas-trabalho/rais/rais-2022/4-tabelas_rais-2022.xlsx"

  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Download the file
  file_path <- .download_file(url_2022, dir_path)
}
