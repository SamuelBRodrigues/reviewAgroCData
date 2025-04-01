#' Extrai os dados de Estabelecimentos Agropecuários que recebem Assistência Técnica
#'
#' A função extrai  dados de Estabelecimentos Agropecuários que recebem Assistência Técnica
#' do Senso Agro de 2017 através da API do SIDRA IBGE.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_recebe_assist()
#' }
get_recebe_assist <- function(){

  vetor_municipios_alvo <- "5107875,5107925,2928901,5103502,5102637,5218805,2911105,5102678,5106224,5211909,5106240,5206206,4306601,5107065,5102686,5107040,2903201,5105259,2909307,2101400,2112001,5006606,5005400,3170404,5102702,2919553,5107958,3170107,5108006,5101902,1508126,5104526,5106307,5007901,5008305,5104609,5007208,2201150,5007109,2708105,4104808,2700300,3147006,2211209,5003256,5213756,4322202,5003702,1708205,5107859"
  api_req <- stringr::str_glue("/t/6779/n6/{vetor_municipios_alvo}/v/all/p/all/c829/46302/c12567/41151,113111/c12564/41145/c218/46502/c12771/45951/c800/41147")

  perc_recebe_assist <- sidrar::get_sidra(api = api_req) |>
    janitor::clean_names() |>
    tibble::tibble() |>
    dplyr::select(
      municipio_codigo,
      municipio,
      ano,
      valor,
      origem_da_orientacao_tecnica_recebida
    ) |>
    tidyr::pivot_wider(
      names_from = origem_da_orientacao_tecnica_recebida,
      values_from = valor
    ) |>
    dplyr::mutate(
      perc_origem_da_orientacao_tecnica_recebida = Recebe/Total * 100
    )

  return(perc_recebe_assist)

}
