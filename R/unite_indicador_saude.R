#' Une as tabelas que compõe o indicador de saúde
#'
#' Essa função une as tabelas que compões o indicador de saúde em um único dataframe
#'
#' @param cobertura_aps Tabela com os dados de Cobertura de Assistência Primária
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
#' @param cobertura_ab Tabela com os dados de Cobertura de Assistência Básica
#'
#' @returns Dataframe com todas as variáveis que compõe o indicador de saúde
#' @export
#'
#' @examples
unite_indicador_saude <- function(
    cobertura_aps, cobertura_ab, gastos_per_capta, mortalidade_infantil, obitos_evitaveis,
    abastecimento_esgoto, cobertura_vacinal, equip_por_estab, internacoes, obitos,
    subnutricao, obesidade
){
  data <- target_cities |>
    dplyr::select(
      -populacao
    ) |>
    dplyr::left_join(
      cobertura_aps |>
        dplyr::select(
          -municipio
        )
    ) |>
    dplyr::left_join(
      cobertura_ab |>
        dplyr::select(
          -municipio
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
          -dplyr::starts_with("x"), -ano
        )
    ) |>
    dplyr::left_join(
      abastecimento_esgoto |>
        dplyr::select(
          -c(municipio_nome, ano)
        ) |>
        dplyr::mutate(
          municipio_codigo = as.character(municipio_codigo)
        )
    ) |>
    dplyr::left_join(
      cobertura_vacinal |>
        dplyr::select(
          -c(municipio, ano, cobertura_vacinal_max)
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
          -c(municipio, uf, year,)
        )
    ) |>
    dplyr::left_join(
      internacoes |>
        dplyr::select(
          -c(uf, year, municipio)
        )
    ) |>
    dplyr::left_join(
      obitos |>
        dplyr::select(
          -c(uf, year, municipio)
        )
    ) |>
    dplyr::left_join(
      subnutricao |>
        dplyr::select(
          municipio_codigo, crianca_0_10, adultos_idoso_gestantes, adolescentes
        )
    ) |>
    dplyr::left_join(
      obesidade |>
        dplyr::select(
          municipio_codigo, pop_obesa
        )
    )

  return(data)
}
