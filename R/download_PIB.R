#' Download PIB Data (2010-2021) from IBGE
#'
#' This function downloads the PIB (Produto Interno Bruto) dataset for the years 2010-2021 from the IBGE FTP server and saves it to a specified directory.
#'
#' @param dir_path Character. The destination directory where the downloaded ZIP file will be saved. Default is "data/".
#'
#' @return No return value, called for side effects (file download).
#'
#' @examples
#' # Download the PIB dataset and save it in the default directory ("data/")
#' download_PIB()
#'
#' # Download the PIB dataset and save it in the working directory
#' download_PIB(".")
#'
#' # Download the PIB dataset and save it in a specific folder
#' download_PIB("custom_folder/")
#'
#' @export

download_PIB <- function(dir_path = "data/") {
  url <- "https://ftp.ibge.gov.br/Pib_Municipios/2021/base/base_de_dados_2010_2021_xlsx.zip"
  file_name <- "base_de_dados_2010_2021_xlsx.zip"
  file_path <- file.path(dir_path, file_name)
  
  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Download the file
  response <- tryCatch(
    {
      download.file(url, destfile = file_path, mode = "wb")
      message("Download completed: ", file_path)
    },
    error = function(e) {
      message("Error downloading the file: ", e$message)
    }
  )
}