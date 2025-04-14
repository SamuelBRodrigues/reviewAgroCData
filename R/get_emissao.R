#' Extrai os dados de Emissão por População
#'
#' A função extra os dados de Emissão por População do SEEG Municípios.
#'
#' @returns Retorna uma tabela com os dados de Emissão por Populção a nível municipal
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_emissao()
#' }
get_emissao <- function(){

  emissao <-  emissao_2019 |>
    dplyr::mutate(
      dplyr::across(-c(`CÓD. IBGE`,Município, UF),
                    ~ tidyr::replace_na(.x, 0)
      )
    ) |>
    dplyr::mutate(
      emissao_por_pop = AC + AL + AM + AP + BA + CE + DF + ES + GO + MA + MG + MS +
        MT + PA + PB + PE + PI + PR + RJ + RN + RO + RR + RS + SC + SE + SP + TO
    ) |>
    janitor::clean_names() |>
    dplyr::select(
      cod_ibge, municipio, uf, emissao_por_pop
    )

  return(emissao)

}
