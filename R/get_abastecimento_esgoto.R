#' Extração dos dados de Abastecimento de Água e Esgotamento Sanitário
#'
#' A função extrai os dados de Abastecimento de Água e Esgotamento Sanitário a
#' nível municipal para o ano de 2021 diretamente da plataforma do Instituto Água
#' e Saneamento.
#'
#' @returns Uma dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data_abatecimento_e_esgoto <- get_abastecimento_esgoto()
#' }
get_abastecimento_esgoto <- function(){

  # Construindo a requisição para acessar os dados de Abastecimento e Esgoto dos
  # Municípios

  req <- httr2::request("https://www.aguaesaneamento.org.br/municipios-e-saneamento/exploreCustomCSV") |>
    httr2::req_url_query(
      v2 = "",
    ) |>
    httr2::req_headers(
      accept = "text/plain, */*; q=0.01",
      `accept-language` = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
      `content-type` = "application/x-www-form-urlencoded; charset=UTF-8",
      cookie = "_gid=GA1.3.1944777815.1743187811; _ga=GA1.1.937080257.1743187811; _hjSessionUser_1925616=eyJpZCI6IjIzOWJjNzk1LTg3MGEtNWEyMi1iNmIxLWEyYmM1NDk0ZDMwYSIsImNyZWF0ZWQiOjE3NDMxODc4MTUzNzcsImV4aXN0aW5nIjp0cnVlfQ==; _hjSession_1925616=eyJpZCI6ImZjYjQxZTA1LTc4ZTEtNDdiNS1hMmYwLTQyYTk1NjVhNDU2ZiIsImMiOjE3NDMxODc4MTUzODAsInMiOjEsInIiOjEsInNiIjowLCJzciI6MCwic2UiOjAsImZzIjoxLCJzcCI6MH0=; _ga_GTTPTZB4XM=GS1.1.1743187814.1.1.1743189223.2.0.1817436974",
      origin = "https://www.aguaesaneamento.org.br",
      priority = "u=1, i",
      `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36 OPR/117.0.0.0",
      `x-requested-with` = "XMLHttpRequest",
    ) |>
    httr2::req_body_raw("filtro=cidade||asc&estado=&bioma=&faixa=&politica=&agrupamento=&snis=&rm=&indicador=2|3|&aglom=&ride=", "application/x-www-form-urlencoded; charset=UTF-8")

  # Performando a requisição
  resp <- httr2::req_perform(req)

  # Estruturando a resposta em uma tabela csv2
  csv_table <- resp |>
    httr2::resp_body_string() |>
    readr::read_csv2()

  # Tratando e estruturando os dados
  data <- csv_table |>
    janitor::clean_names() |>
    dplyr::select(
      codigo_ibge, cidade,
      indice_da_populacao_total_atendida_com_esgotamento_sanitario_percent,
      indice_da_populacao_total_atendida_com_abastecimento_de_agua_percent
    ) |>
    dplyr::rename(
      "municipio_codigo" = codigo_ibge,
      "municipio_nome" = cidade
    ) |>
    dplyr::mutate(
      ano = "2021"
    )

  return(data)
}
