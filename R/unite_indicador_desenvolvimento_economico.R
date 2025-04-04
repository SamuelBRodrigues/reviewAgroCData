#' Une as tabelas que compõe o indicador de desenvolvimento econômico
#'
#' Essa função une as tabelas que compões o indicador de desenvolvimento econômico em um único dataframe
#'
#' @param pib Tabela com os dados de PIB Municipais
#' @param empregos Tabela com os dados Emprego por 100 mil Habitatnes
#' @param ivs Tabela com os dados Índice de Vulnerabilidade Social
#' @param investimento_per_capita Tabela com os dados
#' @param recebe_assist Tabela com os dados
#' @param produtividade_agricola Tabela com os dados
#' @param perc_produtores_jovens Tabela com os dados
#' @param perc_pessoas_parentesco Tabela com os dados
#' @param prop_sistemas_florestais Tabela com os dados
#' @param prop_boas_pastagens Tabela com os dados
#' @param empresas_abertas Tabela com os dados de empresas
#'
#' @returns Dataframe com todas as variáveis que compõe o indicador de desenvolvimento econômico
#' @export
#'
#' @examples
unite_indicador_desenvolvimento_economico <- function(
    pib, empregos, ivs, investimento_per_capita, recebe_assist, produtividade_agricola,
    perc_produtores_jovens, perc_pessoas_parentesco, prop_sistemas_florestais,
    prop_boas_pastagens, empresas_abertas
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
          municipio_codigo, PIB_per_capta_2021
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
          muncipio_codigo, investimento_per_capita
        ),
      by = join_by(municipio_codigo == muncipio_codigo)
    ) |>
    dplyr::left_join(
      ivs |>
        dplyr::select(
          municipio_codigo, ivs_infraestrutura_urbana, ivs_renda_e_trabalho
        )
    ) |>
    dplyr::left_join(
      recebe_assist |>
        dplyr::select(
          municipio_codigo, perc_origem_da_orientacao_tecnica_recebida
        )
    ) |>
    dplyr::left_join(
      produtividade_agricola |>
        dplyr::select(
          municipio_codigo, valor_da_producao_das_lavouras_temporarias,
          area_colhida_nas_lavouras_temporarias, prod_por_hec_lavouras_temporarias
        )
    ) |>
    dplyr::left_join(
      perc_produtores_jovens |>
        dplyr::select(
          municipio_codigo, perc_prod_jovens_25_35_anos
        )
    ) |>
    dplyr::left_join(
      perc_pessoas_parentesco |>
        dplyr::select(
          municipio_codigo, perc_estabelecimento_agrop_ocup_laço_parentesco_prod
        )
    ) |>
    dplyr::left_join(
      prop_sistemas_florestais |>
        dplyr::select(
          municipio_codigo, perc_producao_florestal_florestas_nativas
        )
    ) |>
    dplyr::left_join(
      prop_boas_pastagens |>
        dplyr::select(
          municipio_codigo, perc_pastagens_plantadas_em_boas_condicoes
        )
    ) |>
    dplyr::left_join(
      empresas_abertas |>
        dplyr::select(-municipio_nome)
    )
  return(data)
}
