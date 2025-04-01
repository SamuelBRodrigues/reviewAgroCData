#' Extrai os dados de Estabelecimento Agropecuário dirigidos por Jovens
#'
#' A função extrai  dados de Estabelecimento Agropecuário dirigidos por Jovens
#' entre 25 e 35 anos do Senso Agro de 2017 através da API do SIDRA IBGE.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_perc_prod_jovens_25_35_anos()
#' }
get_perc_prod_jovens_25_35_anos <- function(){
  vetor_municipios_alvo <- "5107875,5107925,2928901,5103502,5102637,5218805,2911105,5102678,5106224,5211909,5106240,5206206,4306601,5107065,5102686,5107040,2903201,5105259,2909307,2101400,2112001,5006606,5005400,3170404,5102702,2919553,5107958,3170107,5108006,5101902,1508126,5104526,5106307,5007901,5008305,5104609,5007208,2201150,5007109,2708105,4104808,2700300,3147006,2211209,5003256,5213756,4322202,5003702,1708205,5107859"
  api_req <- stringr::str_glue("/t/6755/n6/{vetor_municipios_alvo}/v/all/p/all/c829/46302/c12564/41145/c800/41147/c840/46586/c830/46427/c12771/45951,118191")


  perc_prod_jovens_25_35_anos <- sidrar::get_sidra(api = api_req) |>
    janitor::clean_names() |>
    tibble::tibble() |>
    dplyr::select(
      municipio_codigo,
      municipio,
      ano,
      valor,
      classe_de_idade_do_produtor
    ) |>
    tidyr::pivot_wider(
      names_from = classe_de_idade_do_produtor,
      values_from = valor
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      perc_prod_jovens_25_35_anos = de_25_a_menos_de_35_anos/total * 100
    )

  return(perc_prod_jovens_25_35_anos)
}
