#' Extração dos dados de Tempo de Deslocamento de Casa/Trabalho
#'
#' Essa função extrai os dados de Tempo Habitual de deslocamento de Casa para o
#' Trabalho a nível municípal de um determinado ano diretamente da API do SIDRA.
#'
#' @param ano Ano de interesse dos dados. Por padrão utiliza 2010 devido a ser o
#' único ano disponível
#'
#' @returns Uma dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'  dados_deslocamento <- get_deslocamento_SIDRA(ano = 2010)
#' }
get_deslocamento_SIDRA <- function(ano = 2010){

  deslocamento <- sidrar::get_sidra(api = '/t/3422/n6/all/v/1001319/p/all/c490/12829,12830/d/v1001319%202') |>
    janitor::clean_names() |>
    dplyr::select(
      municipio_codigo, municipio, variavel, ano, tempo_habitual_de_deslocamento_para_o_trabalho, valor
    ) |>
    tidyr::pivot_wider(
      names_from = tempo_habitual_de_deslocamento_para_o_trabalho,
      values_from = valor
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      "Percentual da população que gasta 1hora ou mais no deslocamento casatrabalho (total e por faixa de renda)" =
        mais_de_uma_hora_ate_duas_horas + mais_de_duas_horas
    ) |>
    dplyr::filter(ano == ano)
  return(deslocamento)

}
