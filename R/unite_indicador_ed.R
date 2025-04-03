#' Une as tabelas que compõe o indicador de educação
#'
#' Essa função une as tabelas que compões o indicador de educação em um único dataframe
#'
#' @param ideb_finais Tabela com os dados do IDEB
#' @param ideb_iniciais Tabela com os dados do IDEB
#' @param tdi Tabela com os dados da Taxa de Distorção da Idade por Séries
#' @param abandono_escolar Tabela com os dados da Taxa de Abandono Escolar
#' @param matricula_creche Tabela com os dados da Taxa Líquida das Matrículas em Creches
#' @param gastos_educacao Tabela com os dados de Gastos Per Capita em Educação
#' @param gestao Tabela com os dados de Despesas em Educação
#' @param perc_agricultor_ens_bas Tabela com os dados do Percentual de Produtores
#' Agrícolas que concluíram o ensino básico
#' @param perc_alimentos_pnae Tabela com os dados do Percentual de Alimentos da
#' PNAE adquiridos da Agricultura Familiar
#'
#' @returns Dataframe com todas as variáveis que compõe o indicador de educação
#' @export
#'
#' @examples
unite_indicador_ed <- function(
    ideb_finais, ideb_iniciais, tdi, abandono_escolar, matricula_creche, gastos_educacao,
    gestao, perc_agricultor_ens_bas, perc_alimentos_pnae
){
  data <- target_cities |>
    dplyr::select(
      -populacao
    ) |>
    dplyr::left_join(
      ideb_finais |>
        dplyr::select(
          municipio_codigo,
          ideb_2023_anos_finais
        ) |>
        dplyr::mutate(
          ideb_2023_anos_finais = as.numeric(ideb_2023_anos_finais)
        )
    ) |>
    dplyr::left_join(
      ideb_iniciais |>
        dplyr::select(
          municipio_codigo,
          ideb_2023_anos_iniciais
        ) |>
        dplyr::mutate(
          ideb_2023_anos_iniciais = as.numeric(ideb_2023_anos_iniciais)
        )
    ) |>
    dplyr::left_join(
      tdi |>
        dplyr::select(
          municipio_codigo,
          tdi_ensino_fundamental_2023,
          tdi_ensino_medio_2023
        ) |>
        dplyr::mutate(
          municipio_codigo = as.character(municipio_codigo),
          tdi_ensino_fundamental_2023 = as.numeric(tdi_ensino_fundamental_2023),
          tdi_ensino_medio_2023 = as.numeric(tdi_ensino_medio_2023)
        )
    ) |>
    dplyr::left_join(
      abandono_escolar |>
        dplyr::select(
          municipio_codigo,
          taxa_abandono_fundamental_2023,
          taxa_abandono_medio_2023
        ) |>
        dplyr::mutate(
          municipio_codigo = as.character(municipio_codigo)
        )
    ) |>
    dplyr::left_join(
      matricula_creche |>
        dplyr::select(
          municipio_codigo,
          taxa_liquida_matricula_creche_2023
        ) |>
        dplyr::mutate(
          municipio_codigo = as.character(municipio_codigo)
        )
    ) |>
    dplyr::left_join(
      gastos_educacao |>
        tidyr::pivot_wider(
          names_from = ano,
          names_prefix = "gastos_per_capita_educacao_",
          values_from = gastos_per_capita_educacao
        ) |>
        dplyr::select(
          -municipio_nome
        )
    ) |>
    dplyr::left_join(
      gestao |>
        dplyr::select(
          municipio_codigo, despesas_educacao_2023, gestao_dr_vs_de_2023
        )
    ) |>
    dplyr::left_join(
      perc_agricultor_ens_bas |>
        dplyr::select(
          municipio_codigo, perc_agricultor_ens_bas
        )
    ) |>
    dplyr::left_join(
      perc_alimentos_pnae |>
        dplyr::select(municipio_codigo, prop_alimentos_pnae)
    )
  return(data)
}
