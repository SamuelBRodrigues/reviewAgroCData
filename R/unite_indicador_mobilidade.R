#' Une as tabelas que compõe o indicador de mobilidade
#'
#' Essa função une as tabelas que compões o indicador de mobilidade em um único dataframe
#'
#' @param internacoes_transito Tabela com os dados de Internações por Sinistro de
#' Trânsito por 100 mil Habitantes
#' @param obitos_transito Tabela com os dados de Óbitos por Sinistro de
#' Trânsito por 100 mil Habitantes
#' @param deslocamento_sidra Tabela com os dados de Tempo de Deslocamento casa/trabalho
#' superior a uma hora
#' @param emissao Tabela com os dados de Emissão por População
#'
#' @returns Dataframe com todas as variáveis que compõe o indicador de mobilidade
#' @export
#'
#' @examples
unite_indicador_mobilidade <- function(
    internacoes_transito, obitos_transito,deslocamento_sidra,emissao
){
  data <- target_cities |>
    dplyr::select(
      -populacao
    ) |>
    dplyr::left_join(
      deslocamento_sidra |>
        dplyr::select(
          -c(municipio, variavel, ano, mais_de_duas_horas, mais_de_uma_hora_ate_duas_horas)
        )
    ) |>
    dplyr::left_join(
      emissao |>
        dplyr::select(
          -c(municipio_nome)
        )
    ) |>
    dplyr::left_join(
      internacoes_transito |>
        dplyr::select(
          -c(uf, year, municipio)
        )
    ) |>
    dplyr::left_join(
      obitos_transito |>
        dplyr::select(
          -c(uf, year, municipio)
        )
    )
  return(data)
}
