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
  file_name <- "pib_data.zip"  # Simpler name to avoid issues
  file_path <- file.path(dir_path, file_name)
  
  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  tryCatch({
    # Download using automatic method
    download.file(url, destfile = file_path, mode = "wb", method = "auto")
    
    # Check if the download was successful
    if (!file.exists(file_path) || file.info(file_path)$size == 0) {
      stop("Download failed or the file is empty")
    }
    
    message("Download completed: ", file_path)
    
    # Robust extraction with special character handling
    if (requireNamespace("zip", quietly = TRUE)) {
      # Use 'zip' package, which is more robust for special characters
      zip::unzip(file_path, exdir = dir_path)
    } else {
      # Alternative for systems without the 'zip' package
      utils::unzip(file_path, exdir = dir_path)
    }
    
    message("ZIP file extracted successfully.")
    
    # Remove the ZIP file after extraction
    file.remove(file_path)
    message("ZIP file removed.")
    
  }, error = function(e) {
    message("Error during the process: ", e$message)
    # Remove the corrupted ZIP file if it exists
    if (file.exists(file_path)) file.remove(file_path)
  })
}
