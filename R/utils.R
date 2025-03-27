#' Download a file from a given URL
#'
#' @param url Character. The URL of the file to download.
#' @param dir_path Character. The directory where the file should be saved. Default is "data_raw/".
#' @return Character. The path to the downloaded file, or NULL if the download fails.
#' @examples
#' file_path <- .download_file("https://example.com/data.zip")
.download_file <- function(url, dir_path = "data_raw/") {
  file_name <- basename(url)
  file_path <- file.path(dir_path, file_name)

  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  tryCatch({
    # Download file
    download.file(url, destfile = file_path, mode = "wb", method = "auto")

    # Check if the download was successful
    if (!file.exists(file_path) || file.info(file_path)$size == 0) {
      stop("Download failed or the file is empty")
    }

    message("Download completed: ", file_path)
    return(file_path)

  }, error = function(e) {
    message("Error during the download: ", e$message)
    if (file.exists(file_path)) file.remove(file_path)
    return(NULL)
  })
}


#' Extract a ZIP file to a specified directory
#'
#' @param file_path Character. The path to the ZIP file to be extracted.
#' @param dir_path Character. The directory where the contents should be extracted. Default is "data_raw/".
#' @return NULL. The function extracts the files.
#' @examples
#' .extract_zip("data_raw/data.zip")
.extract_zip <- function(file_path, dir_path = "data_raw/") {
  tryCatch({
    # Extract ZIP file
    if (requireNamespace("zip", quietly = TRUE)) {
      zip::unzip(file_path, exdir = dir_path)
    } else {
      utils::unzip(file_path, exdir = dir_path)
    }

    message("ZIP file extracted successfully.")

  }, error = function(e) {
    message("Error during extraction: ", e$message)
  })
}

#' Remove a ZIP file after extraction
#'
#' @param file_path Character. The path to the ZIP file to be removed.
#' @return NULL. The function removes the ZIP file if it exists.
#' @examples
#' .remove_zip("data_raw/data.zip")
.remove_zip <- function(file_path) {
  tryCatch({
    if (file.exists(file_path)) {
      file.remove(file_path)
      message("ZIP file removed.")
    } else {
      message("ZIP file not found: ", file_path)
    }

  }, error = function(e) {
    message("Error during removal: ", e$message)
  })
}

