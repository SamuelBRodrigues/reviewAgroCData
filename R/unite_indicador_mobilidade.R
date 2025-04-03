#' Une as tabelas que compõe o indicador de mobolidade
#'
#' Essa função une as tabelas que compões o indicador de mobolidade em um único dataframe
#'
#' @param cobertura_aps Tabela com os dados de Cobertura de Assistência Básica
#' @param gastos_per_capta Tabela com os dados de Gastor Per Capita em mobolidade
#' @param mortalidade_infantil Tabela com os dados Mortalidade Infantil para 1000
#' Nascimentos
#' @param obitos_evitaveis Tabela com os dados de Mortalidade por Causas Evitáveis
#' @param abastecimento_esgoto Tabela com os dados de Acesso a Esgoto e Abastecimento de Águga
#' @param cobertura_vacinal Tabela com os dados de Cobertura Vacinal
#' @param equip_por_estab Tabela com os dados Equipamento Sus por Equipamentos Totais
#' @param internacoes Tabela com os dados Internações em Sinistros de Trânsito por
#' 100 mil habitantes
#' @param obitos Tabela com os dados Óbitos em Sinistros de Trânsito por
#' 100 mil habitantes
#'
#' @returns Dataframe com todas as variáveis que compõe o indicador de mobolidade
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
