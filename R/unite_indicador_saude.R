#' Une as tabelas que compõe o indicador de saúde
#'
#' Essa função une as tabelas que compões o indicador de saúde em um único dataframe
#'
#' @param cobertura_aps Tabela com os dados de Cobertura de Assistência Básica
#' @param gastos_per_capta Tabela com os dados de Gastor Per Capita em Saúde
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
#' @returns Dataframe com todas as variáveis que compõe o indicador de saúde
#' @export
#'
#' @examples
unite_indicador_saude <- function(
    cobertura_aps, gastos_per_capta, mortalidade_infantil, obitos_evitaveis,
    abastecimento_esgoto, cobertura_vacinal, equip_por_estab, internacoes, obitos
){
  data <- target_cities |>
    dplyr::select(
      -populacao
    ) |>
    dplyr::left_join(
      cobertura_aps |>
        dplyr::select(
          municipio_codigo,
          time_period,
          cobertura_aps
        )
    ) |>
    dplyr::left_join(
      mortalidade_infantil |>
        dplyr::select(
          -cod_uf, -ano
        )
    ) |>
    dplyr::left_join(
      obitos_evitaveis |>
        dplyr::select(
          municipio_codigo,
          total_menor_5,
          total_maior_5,
          mortes_evitaveis
        )
    ) |>
    dplyr::left_join(
      abastecimento_esgoto |>
        dplyr::select(
          municipio_codigo,
          indice_da_populacao_total_atendida_com_esgotamento_sanitario_percent ,
          indice_da_populacao_total_atendida_com_abastecimento_de_agua_percent
        ) |>
        dplyr::mutate(
          municipio_codigo = as.character(municipio_codigo)
        )
    ) |>
    dplyr::left_join(
      cobertura_vacinal |>
        dplyr::select(
          municipio_codigo,
          cobertura_vacinal_municipio
        )
    ) |>
    dplyr::left_join(
      gastos_per_capta |>
        tidyr::pivot_wider(
          names_from = ano,
          names_prefix = "valor_percapta_saude_",
          values_from = valor_percapta_saude
        ) |>
        dplyr::select(
          -municipio
        )
    ) |>
    dplyr::left_join(
      equip_por_estab |>
        dplyr::select(
          municipio_codigo, equipamentos_existentes, estab_c_equip_sus,
          perc_equip_sus_nos_estab_saude
        )
    ) |>
    dplyr::left_join(
      internacoes  |>
        dplyr::select(
          municipio_codigo, internacoes, populacao, taxa_internacoes_100k_hab
        )
    ) |>
    dplyr::left_join(
      obitos |>
        dplyr::select(
          municipio_codigo, obitos, populacao, taxa_obito_100k_hab
        )
    )
  return(data)
}
