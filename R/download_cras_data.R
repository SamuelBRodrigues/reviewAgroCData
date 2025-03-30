#' Download dos dados do SUAS - CRAS
#'
#' A função baixa os dados do CRAS da bases do senso SUAS
#'
#' @param dir diretório onde os dados serão baixados
#' @param ano Ano de interesse dos dados
#'
#' @returns Um arquivo compactado no diretorio indicado
#' @export
#'
#' @examples
#' \dontrun{
#'   download_cras_data(dir = "data_raw", ano = "2022")
#' }
download_cras_data <- function(dir = "data_raw", ano = 2023){

  ano <- ano |> as.character()

  # Lista nomeada com os links de download dos dados
  links <- list(
    "2023" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/1%20-%20CRAS(4).rar",
    "2022" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/1_CRAS.rar",
    "2021" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/1%20-%20CRAS(1).zip",
    "2020" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/1_CRAS_2020.zip",
    "2019" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(5).zip",
    "2018" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(3).zip",
    "2017" = "http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS/2017/Censo_SUAS_2017_CRAS.zip",
    "2016" = "http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS_2016/CensoSUAS2016_CRAS.zip",
    "2015" = "http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS_2015/CensoSUAS2015_CRAS.zip"
  )

  # Url do download de acordo com o ano
  url <- links[ano] |> purrr::pluck(1)

  # Tipo do arquivo baixado
  file_type <- url |> stringr::str_extract("\\.(rar|zip)$")

  # Nome do arquivo baixado
  file_name <- stringr::str_glue("censo_suas_cras_{ano}{file_type}")

  # Caminho do arquivo
  path <- stringr::str_glue("{dir}/cras/{ano}")

  if(!dir.exists(path)){
    dir.create(path, recursive = TRUE)
  }

  # Baixando os dados
  options(timeout = 600) #aumentando o tempo limite de donwload
  download.file(url,
                mode = "wb",
                destfile = stringr::str_glue("{path}/{file_name}")
  )

}
