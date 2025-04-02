#' Download dados da RAIS (Relação Anual de Informações Sociais)
#'
#' Esta função baixa os dados anuais da RAIS diretamente do portal do Ministério do Trabalho e Emprego.
#' Os dados são disponibilizados em formato Excel (.xlsx) contendo tabelas consolidadas.
#'
#' @param dir_path Caminho do diretório onde o arquivo será salvo. Padrão: "data_raw/RAIS/"
#' @param ano Ano dos dados a serem baixados (como string ou numérico). Anos disponíveis: 2020-2024 (obs: 2024 é uma tabela parcial)
#'
#'
#' @return Retorna invisivelmente o caminho completo do arquivo baixado (invisível)
#'
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Baixar dados de 2022 (padrão)
#' download_RAIS()
#'
#' # Baixar dados de 2020 para um diretório específico
#' download_RAIS(dir_path = "dados/rais/", ano = 2020)
#' }
#'
#' @export
download_RAIS <- function(dir_path = "data_raw/RAIS/", ano = "2022") {

  url_map <- list(
    '2024' = 'https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/estatisticas-trabalho/rais/rais-2024/rais-2024-parcial/tabelas-rais-2024-parcial.xlsx',
    '2023' = 'https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/estatisticas-trabalho/rais/rais-2023/tabelas_2022x2023_12-12-2024.xlsx',
    '2022' = "https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/estatisticas-trabalho/rais/rais-2022/4-tabelas_rais-2022.xlsx",
    '2021' = 'https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/estatisticas-trabalho/rais/rais-2021/3-tabelas.xlsx',
    '2020' = 'https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/estatisticas-trabalho/rais/rais-2020/3-tabelas.xlsx'
  )

  ano <- as.character(ano)

  if (!(ano %in% names(url_map))) {
    stop("Ano inválido. Os anos disponíveis são: ",
         paste(names(url_map), collapse = ", "))
  }

  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    message("Diretório criado: ", normalizePath(dir_path))
  }

  url <- url_map[[ano]]
  file_name <- glue::glue('tabela_rais_{ano}.xlsx')
  dest_file <- file.path(dir_path, file_name)

  tryCatch({
    utils::download.file(url = url, destfile = dest_file, mode = "wb")
    message("Arquivo baixado com sucesso: ", normalizePath(dest_file))
    invisible(dest_file)
  }, error = function(e) {
    stop("Falha ao baixar o arquivo. Erro: ", e$message)
  })
}
