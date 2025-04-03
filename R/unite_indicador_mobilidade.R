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
          municipio_codigo,
          `Percentual da população que gasta 1hora ou mais no deslocamento casatrabalho (total e por faixa de renda)`
        )
    ) |>
    dplyr::left_join(
      emissao |>
        dplyr::select(
          cod_ibge, emissao_por_pop
        ),
      by = join_by(municipio_codigo == cod_ibge)
    ) |>
    dplyr::left_join(
      internacoes_transito  |>
        dplyr::select(
          municipio_codigo, internacoes, populacao, taxa_internacoes_100k_hab
        )
    ) |>
    dplyr::left_join(
      obitos_transito |>
        dplyr::select(
          municipio_codigo, obitos, populacao, taxa_obito_100k_hab
        )
    )
  return(data)
}
