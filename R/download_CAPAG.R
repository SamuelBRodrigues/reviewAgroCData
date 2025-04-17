#' Baixa arquivos da Capacidade de Pagamento (CAPAG) de municípios
#'
#' @description
#' Esta função realiza o download dos arquivos da CAPAG (Capacidade de Pagamento),
#' um indicador que avalia a capacidade financeira de estados e municípios brasileiros.
#' Os dados são obtidos do Tesouro Transparente e salvos localmente no formato XLSX.
#'
#' @param dir_path Caminho do diretório de destino para os arquivos.
#'   Padrão: "data_raw/CAPAG/" (será criado se não existir)
#' @param ano Ano base dos dados como string ou numérico (2017-2023).
#'   Padrão: "2025" (último ano disponível)
#'
#' @return Retorna invisivelmente o caminho completo do arquivo baixado.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download do ano padrão (2023)
#' arq_2023 <- download_CAPAG()
#'
#' # Download para ano específico
#' arq_2019 <- download_CAPAG(ano = "2019")
#'
#' # Especificando diretório customizado
#' arq_2021 <- download_CAPAG(dir_path = "dados/financeiros/", ano = 2021)
#' }
download_CAPAG <- function(dir_path = "data_raw/CAPAG/", ano = "2025") {
  anos_disponiveis <- as.character(2017:2025)
  ano <- as.character(ano)

  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ",
         paste(anos_disponiveis, collapse = ", "))
  }

  urls <- list(
    '2017' = 'https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/379e92f3-d8a9-4aa6-a23c-ca82a8f5b026/download/CAPAG-Municipios---2018.xlsx',
    '2018' = 'https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/444b8cb5-6fea-4907-82d9-018aae1d68b5/download/CAPAG-2019---Municipios.xlsx',
    '2019' = 'https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/f2149990-1ca4-475d-95c1-512f78079905/download/CAPAG-Municipios.xlsx',
    '2020' = 'https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/10387f66-f44d-4096-811c-58e68503100a/download/Capag-Municipios---novembro-2021.xlsx',
    '2021' = 'https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/6a218451-f1b4-4fce-ac2a-00a3675bf4eb/download/CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx',
    '2022' = 'https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/31ed778a-9115-419c-b18e-c9131a978aef/download/CAPAG-Municipios-2023.xlsx',
    '2023' = 'https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/30c5fc20-634d-4558-9d45-01645b501deb/download/20241015CAPAG-Municipios.xlsx',
    '2024' = 'https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/30c5fc20-634d-4558-9d45-01645b501deb/download/20241015CAPAG-Municipios.xlsx',
    '2025' = 'https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/986ec09d-f867-40e2-a466-0f525217df20/download/CAPAG-Municipios-posicao-2025-fev-19.xlsx'
  )

  if (!ano %in% names(urls)) {
    stop("Ano não disponível. Opções: ", paste(names(urls), collapse = ", "))
  }

  url <- urls[[ano]]

  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  file_path <- .download_file(url, dir_path)

  invisible(file_path)
}

