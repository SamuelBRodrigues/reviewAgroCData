#' Extrai os dados de Emissão por População
#'
#' A função extra os dados de Emissão por População do SEEG Municípios.
#'
#' @param ano Ano de interesse dos dados. Por padrão usa o mais recente, 2019.
#'
#' @returns Retorna uma tabela com os dados de Emissão por Populção a nível municipal
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_emissao()
#' }
get_emissao <- function(ano = 2019){

  ano <- as.character(ano)

  data <- emissao_completo |>
    janitor::clean_names() |>
    dplyr::filter(ano1 == ano) |>
    dplyr::group_by(
      cod_ibge, municipio, ano1
    ) |>
    dplyr::summarise(
      emissao_populacao = sum(emissao_populacao,na.rm = T)
    ) |>
    dplyr::rename(
      municipio_codigo = cod_ibge,
      municipio_nome = municipio,
      !!stringr::str_glue("emissao_populacao_{ano}") := emissao_populacao
    ) |>
    dplyr::select(
      -ano1
    )
}
