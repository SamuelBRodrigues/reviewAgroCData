#' Trata os dados do CRAS
#'
#' A função trata os dados do CRAS já baixados e extraídos no mesmo diretório
#' indicado de download
#'
#' @param dir Diretório onde os dados foram baixados
#' @param ano Ano de interesse dos dados a serem tratados
#'
#' @returns Uma tabela com os dados do CRAS a nível municipal
#' @export
#'
#' @examples
#' \dontrun{
#'   extract_cras_data(dir = "data_raw", ano = "2022")
#' }
extract_cras_data <- function(dir = "data_raw", ano = 2022){

  if(ano >= 2022){
    extrai_rar <- function(arquivo_rar, destino = ".") {
      # Verifica se o 7z está disponível
      seven_zip <- if (.Platform$OS.type == "windows") {
        # Caminho padrão do 7z no Windows (ajuste se necessário)
        possible_paths <- c(
          "C:/Program Files/7-Zip/7z.exe",
          "C:/Program Files (x86)/7-Zip/7z.exe"
        )
        found <- possible_paths[file.exists(possible_paths)]
        if (length(found) > 0) found[1] else "7z"
      } else {
        "7z"
      }

      # Monta o comando
      cmd <- sprintf('"%s" x "%s" -o"%s" -y', seven_zip, arquivo_rar, destino)

      # Executa
      status <- system(cmd, intern = TRUE)

      # Verifica se houve erro
      if (any(grepl("ERROR", status, ignore.case = TRUE))) {
        stop("Erro ao extrair o arquivo .rar: ", paste(status, collapse = "\n"))
      }

      return(invisible(TRUE))
    }
    path_rar <- stringr::str_glue("{dir}/cras/{ano}/censo_suas_cras_{ano}.rar")
    extrai_rar(path_rar,
               stringr::str_glue("{dir}/cras/{ano}/"))
  } else{
    path_zip <- stringr::str_glue("{dir}/cras/{ano}/censo_suas_cras_{ano}.zip")
    zip::unzip(path_zip,
               exdir = stringr::str_glue("{dir}/cras/{ano}/")
    )
  }
  ano <- ano |> as.character()

  file_folder_list <- list(
    "2023" = "1 - CRAS",
    "2022" = "1_CRAS",
    "2021" = "1 - CRAS",
    "2020" = "",
    "2019" = "CRAS",
    "2018" = "1.CRAS",
    "2017" = "Censo_SUAS_2017_CRAS",
    "2016" = "",
    "2015" = "CRAS"
  )

  file_folder <- file_folder_list[ano] |> purrr::pluck(1)

  if(ano %in% c("2016", "2020")){
    path <- stringr::str_glue("{dir}/cras/{ano}")
  } else{
    path <- stringr::str_glue("{dir}/cras/{ano}/{file_folder}")
  }

  if(ano %in% c("2020", "2021", "2023")){
    file_path <- list.files(path,
                            pattern = "ais.(xlsx|sav)",
                            full.names = T)
  } else{
    file_path <- list.files(path,
                            pattern = "(O|o).xls",
                            full.names = T) |>
      purrr::pluck(1)
  }

  if(ano == "2023"){
    data <- haven::read_sav(file_path)
  } else{
    data <- readxl::read_xlsx(file_path)
  }

  data <- data |>
    dplyr::rename(
      "cras" = q01,
      "municipio_nome" = q09,
      "estado_sigla" = q010
    ) |>
    dplyr::group_by(
      estado_sigla, municipio_nome
    ) |>
    dplyr::summarise(
      cras = n()
    ) |>
    dplyr::left_join(
      pop_municipios |>
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = ""
        )
      ,
      by = join_by(municipio_nome == nome_do_municipio, estado_sigla == uf)
    ) |>
    dplyr::mutate(
      cras_por_100k_hab = (cras/populacao) * 100000
    ) |>
    dplyr::relocate(
      municipio_codigo, municipio_nome, cras, populacao, cras_por_100k_hab
    )

  return(data)
}
