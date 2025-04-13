#' Proporção de estabelecimentos de agricultura familiar - Desenvolvimento Econômico
#'
#' Essa função pega os dados da Proporção de Estabelecimentos de Agricultura Familiar
#' diretamente do Censo Agro na plataforma do SIDRA
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_percentual_estabelecimentos_agricultura_familiar()
#' }
get_percentual_estabelecimentos_agricultura_familiar <- function(){

  vetor_municipios_alvo <- "5107875,5107925,2928901,5103502,5102637,5218805,2911105,5102678,5106224,5211909,5106240,5206206,4306601,5107065,5102686,5107040,2903201,5105259,2909307,2101400,2112001,5006606,5005400,3170404,5102702,2919553,5107958,3170107,5108006,5101902,1508126,5104526,5106307,5007901,5008305,5104609,5007208,2201150,5007109,2708105,4104808,2700300,3147006,2211209,5003256,5213756,4322202,5003702,1708205,5107859"

  api_req <- stringr::str_glue(
    "/t/6778/n6/{vetor_municipios_alvo}/v/all/p/all/c829/46302,46303,46304/c309/10969/c218/46502/c12553/46523/c12517/113601/c220/110085")

  est_agricultura_familiar <- sidrar::get_sidra(api = api_req) |>
    janitor::clean_names() |>
    tibble::tibble() |>
    dplyr::mutate(
      variavel = stringr::str_glue("{variavel} - {tipologia}"),
    ) |>
    dplyr::select(
      municipio_codigo,
      municipio,
      ano,
      variavel,
      valor
    ) |>
    tidyr::pivot_wider(
      names_from = variavel,
      values_from = valor
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      percent_estabelecimentos_agricultura_familiar = numero_de_estabelecimentos_agropecuarios_total / numero_de_estabelecimentos_agropecuarios_agricultura_familiar_sim*100,
    )

  return(est_agricultura_familiar)

}
