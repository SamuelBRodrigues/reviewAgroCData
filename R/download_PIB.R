#' Download PIB (Produto Interno Bruto) data from IBGE
#'
#' This function downloads the PIB (Produto Interno Bruto) data for municipalities in Brazil
#' for the years 2010-2021 from IBGE's server. It saves the ZIP file, extracts its contents,
#' and removes the temporary ZIP file after extraction.
#'
#' The function ensures that the specified directory exists, performs the download,
#' extracts the files, and cleans up by removing the ZIP file.
#'
#' @param dir_path Path to the directory where the files will be saved (default: "data/").
#'
#' @return Invisibly returns the full path of the downloaded and extracted files.
#'
#' @details
#' The function performs the following steps:
#' 1. Downloads the ZIP file containing PIB data from IBGE's server.
#' 2. Ensures that the specified directory exists, creating it if necessary.
#' 3. Extracts the contents of the ZIP file, using the `zip` package (if available) for better handling of special characters.
#' 4. Removes the temporary ZIP file after extraction.
#'
#' The final extracted data will be saved in the specified directory, with the ZIP file being removed after the process.
#'
#' @examples
#' \dontrun{
#' # Download PIB data to the default directory ("data/")
#' download_PIB()
#'
#' # Download PIB data to a custom directory
#' download_PIB(dir_path = "data/raw/")
#' }
#'
#' @export
download_PIB <- function(dir_path = "data/") {
  url <- "https://ftp.ibge.gov.br/Pib_Municipios/2021/base/base_de_dados_2010_2021_xlsx.zip"

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
