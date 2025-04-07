#' Une as tabelas que compõe o indicador de proteção social
#'
#' Essa função une as tabelas que compões o indicador de proteção social em um único dataframe
#'
#' @param cad_bf Tabela contendo os dados da Proporção de Familias que recebem
#' Bolsa Familia e tem renda de até meio salário Mínimo e a Proporção de Famílias
#' que recebem até meio salário mínimo e estão cadastradas no CadÚnico
#' @param situacao_pobreza Tabela contendo os dados Famílias em situação de pobreza
#' e estão cadastradas no CadÚnico
#' @param cras Tabela contendo os dados de Cras por 100 mil Habitantes
#' @param ivs Tabela contendo os dados de Índice de Vulnerabilidade Social
#' @param perc_estab_dirigidos_por_mulheres Tabela contendo os dados do percentual
#' de Estabelecimentos Agrícolas que são dirigidos por Mulheres
#'
#' @returns Dataframe com todas as variáveis que compõe o indicador de proteção social
#' @export
#'
#' @examples
unite_indicador_protecao_social <- function(
    cad_bf,
    situacao_pobreza,
    cras,
    ivs,
    perc_estab_dirigidos_por_mulheres
){
  data <- target_cities |>
    dplyr::select(
      -populacao
    ) |>
    dplyr::left_join(
      cad_bf |>
        dplyr::select(
          -nome_do_municipio, -ano, -mes
        ),
      by = join_by(municipio_codigo == cod_ibge)
    ) |>
    dplyr::left_join(
      situacao_pobreza |>
        dplyr::select(
          -nome_do_municipio, -ano, -mes
        ),
      by = join_by(municipio_codigo == cod_ibge)
    ) |>
    dplyr::left_join(
      cras |>
        dplyr::select(
          -c(municipio_nome, cras, populacao)
        )
    ) |>
    dplyr::left_join(
      ivs |>
        dplyr::select(
          -c(ano, ivs_capital_humano, ivs_infraestrutura_urbana, ivs_renda_e_trabalho)
        )
    ) |>
    dplyr::left_join(
      perc_estab_dirigidos_por_mulheres |>
        dplyr::select(
          -c(municipio, ano, Total, Homens, Mulheres)
        )
    )
  return(data)
}
