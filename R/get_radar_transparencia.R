#' Baixa e Resume o Radar da Transparência Pública
#'
#' Esta função acessa uma planilha pública no Google Sheets contendo avaliações de transparência pública,
#' resume os dados por município, calcula um índice de transparência e cruza o resultado com uma tabela
#' de cidades-alvo (target_cities).
#'
#' @details
#' A função realiza as seguintes etapas:
#' - Baixa a planilha pública via googlesheets4::read_sheet.
#' - Limpa os nomes das variáveis com janitor::clean_names.
#' - Seleciona as variáveis codigo_municipio, municipio e avaliacao_certificavel.
#' - Resume os dados calculando o número de avaliações e a média de avaliação certificável.
#' - Faz um join com o objeto target_cities (pré-existente no ambiente).
#'
#' @return Um tibble com o resumo dos indicadores de transparência para os municípios selecionados.
#'
#' @examples
#' \dontrun{
#' # Executar a função (target_cities precisa estar carregado no ambiente)
#' resultados <- baixa_radar_transparencia()
#' }
#'
#' @note
#' A função depende da existência do objeto target_cities no ambiente. Certifique-se de que ele esteja carregado antes de executar a função.
#'
#' @importFrom googlesheets4 read_sheet
#' @importFrom janitor clean_names
#' @importFrom dplyr select summarise mutate inner_join
#' @export
get_radar_transparencia <- function() {
  radar_transparencia <- googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1kz8-0rIcLeFEJbVEkLlZ6wuJPvi0nHN7STVKazqLQB0/edit?gid=1567771439#gid=1567771439'
  ) %>%
    janitor::clean_names()

  data <- radar_transparencia %>%
    dplyr::select(codigo_municipio, municipio, avaliacao_certificavel) %>%
    dplyr::summarise(
      .by = c(codigo_municipio, municipio),
      n = n(),
      soma_avalicao = sum(avaliacao_certificavel, na.rm = TRUE),
      indice_transparencia = soma_avalicao / n
    ) %>%
    dplyr::mutate(codigo_municipio = as.character(codigo_municipio)) %>%
    dplyr::inner_join(
      target_cities,
      join_by(
        codigo_municipio == municipio_codigo,
        municipio == municipio_nome
      )
    ) |>
    dplyr::select(
      municipio_codigo = codigo_municipio, municipio_nome = municipio,
      indice_transparencia
    )
  return(data)
}
