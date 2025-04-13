#' Une as tabelas que compõe o indicador de desenvolvimento econômico
#'
#' Essa função une as tabelas que compões o indicador de desenvolvimento econômico em um único dataframe
#'
#' @param pib Tabela com os dados de PIB Municipais
#' @param empregos Tabela com os dados Emprego por 100 mil Habitatnes
#' @param ivs Tabela com os dados Índice de Vulnerabilidade Social
#' @param investimento_per_capita Tabela com os dados
#' @param recebe_assist Tabela com os dados
#' @param perc_produtores_jovens Tabela com os dados
#' @param perc_pessoas_parentesco Tabela com os dados
#' @param prop_sistemas_florestais Tabela com os dados
#' @param prop_boas_pastagens Tabela com os dados
#' @param empresas_abertas Tabela com os dados de empresas
#' @param produtividade_agricola_lt Tabela com os dados de Produtividade Agrícola
#' por hectare das lavouras temporárias
#' @param produtividade_agricola_lp Tabela com os dados de Produtividade Agrícola
#' por hectare das lavouras permanentes
#'
#' @returns Dataframe com todas as variáveis que compõe o indicador de desenvolvimento econômico
#' @export
#'
#' @examples
unite_indicador_desenvolvimento_economico <- function(
    pib, empregos, ivs, investimento_per_capita, recebe_assist, produtividade_agricola_lt,
    produtividade_agricola_lp,perc_produtores_jovens, perc_pessoas_parentesco,
    prop_sistemas_florestais, prop_boas_pastagens, empresas_abertas, perc_estab_agric_familia
){
  data <- target_cities |>
    dplyr::select(
      -populacao
    ) |>
    dplyr::left_join(
      pib |>
        dplyr::mutate(
          municipio_codigo = as.character(municipio_codigo)
        ) |>
        dplyr::select(
          -c(municipio_nome, estado_sigla)
        )
    ) |>
    dplyr::left_join(
      empregos |>
        dplyr::select(
          -municipio_nome, -estado_sigla, -populacao
        )
    ) |>
    dplyr::left_join(
      investimento_per_capita |>
        dplyr::select(
          -c(municipio, ano, variavel)
        ),
      by = join_by(municipio_codigo == muncipio_codigo)
    ) |>
    dplyr::left_join(
      ivs |>
        dplyr::select(
          -c(ano, ivs, ivs_capital_humano)
        )
    ) |>
    dplyr::left_join(
      recebe_assist |>
        dplyr::select(
          -c(municipio, ano, Total, Recebe)
        )
    ) |>
    dplyr::left_join(
      produtividade_agricola_lt |>
        dplyr::select(
          -c(municipio, ano)
        )
    ) |>
    dplyr::left_join(
      produtividade_agricola_lp |>
        dplyr::select(
          -c(municipio, ano,
             valor_da_producao_das_lavouras_permanentes_nos_estabelecimentos_agropecuarios_com_50_pes_e_mais_existentes)
        )
    ) |>
    dplyr::left_join(
      perc_produtores_jovens |>
        dplyr::select(
          -c(municipio, ano, total, de_25_a_menos_de_35_anos)
        )
    ) |>
    dplyr::left_join(
      perc_pessoas_parentesco |>
        dplyr::select(
          -c(municipio, ano, ocupante, total)
        )
    ) |>
    dplyr::left_join(
      prop_sistemas_florestais |>
        dplyr::select(
          -c(municipio, ano, total, producao_area_florestal)
        )
    ) |>
    dplyr::left_join(
      prop_boas_pastagens |>
        dplyr::select(
          -c(municipio, ano, total, pastagens_plantadas_em_boas_condicoes)
        )
    ) |>
    dplyr::left_join(
      empresas_abertas |>
        dplyr::select(-municipio_nome)
    ) |>
    dplyr::left_join(
      perc_estab_agric_familia |>
        dplyr::select(
          -c(municipio, ano)
        )
    )
  return(data)
}
