#' Une as tabelas que compõe o indicador de infraestrutura
#'
#' Essa função une as tabelas que compões o indicador de infraestrutura em um único
#' dataframe
#'
#' @param acess_agua_mun Tabela com os dados de Acesso a Distribuição de Agua dos Municípios
#' @param acess_esgot_mun Tabela com os dados de Acesso a Sistema de Esgoto dos Municípios
#' @param vias_pav_mun Tabela com os dados de Porcentagem de Vias Pavimentadas dos Municípios
#' @param cob_inte_mun Tabela com os dados de Cobertura de Internet Móvel dos Municípios
#' @param conect_rural_mun Tabela com os dados de Indicador de Conectividade Rural dos Municípios
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
unite_indicador_infraestrutura <- function(
    acess_agua_mun, acess_esgot_mun, vias_pav_mun, cob_inte_mun, conect_rural_mun
){
  data <- target_cities |>
    dplyr::left_join(
      acess_agua_mun
    ) |>
    dplyr::left_join(
      acess_esgot_mun
    ) |>
    dplyr::left_join(
      vias_pav_mun
    ) |>
    dplyr::left_join(
      cob_inte_mun
    ) |>
    dplyr::left_join(
      conect_rural_mun
    )
  return(data)
}
