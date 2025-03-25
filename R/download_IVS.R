#' Download and Extract the IVS Data
#'
#' This function downloads a ZIP file from a predefined URL containing municipal data,
#' extracts its contents to a specified directory, and removes the ZIP file afterward.
#' It ensures that the directory exists, handles errors during the download and extraction process,
#' and uses robust extraction methods for files with special characters.
#'
#' @param dir_path A character string specifying the directory path where the data should be downloaded and extracted.
#' Defaults to `"data/"`.
#'
#' @details
#' The function downloads a ZIP file from the IPEA API containing the basecompletamunicipal dataset.
#' After downloading, it attempts to extract the contents of the ZIP file using either the `zip` package (if installed)
#' or the base `unzip` function. After extraction, the ZIP file is deleted.
#'
#' The function handles errors gracefully, displaying messages when the download or extraction fails, and cleaning up
#' corrupted files if necessary.
#'
#' @return This function does not return any value. It performs the download and extraction side-effects.
#'
#' @examples
#' # Download and extract the IVS data to the default directory
#' download_IVS()
#'
#' # Download and extract the IVS data to a custom directory
#' download_IVS(dir_path = "my_data/")
#'
#' @export
download_IVS <- function(dir_path = "data/") {
  url <- "https://api-ivs.ipea.gov.br/cockpit/storage/uploads/databases/basecompletamunicipal.zip"

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
