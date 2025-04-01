#' Extrai os dados de Estabelecimentos Agropecuários em que o Agricultor concluiu o Ensino Básico
#'
#' A função extrai  dados de Estabelecimentos Agropecuários em que o Agricultor
#' concluiu o Ensino Básico do Senso Agro de 2017 através da API do SIDRA IBGE.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_perc_agricultor_ens_bas()
#' }
get_perc_agricultor_ens_bas <- function(){

  vetor_municipios_alvo <- "5107875,5107925,2928901,5103502,5102637,5218805,2911105,5102678,5106224,5211909,5106240,5206206,4306601,5107065,5102686,5107040,2903201,5105259,2909307,2101400,2112001,5006606,5005400,3170404,5102702,2919553,5107958,3170107,5108006,5101902,1508126,5104526,5106307,5007901,5008305,5104609,5007208,2201150,5007109,2708105,4104808,2700300,3147006,2211209,5003256,5213756,4322202,5003702,1708205,5107859"
  api_req <- stringr::str_glue("/t/6755/n6/{vetor_municipios_alvo}/v/all/p/all/c829/46302/c12564/41145/c800/40711,40712,40713,40714,41147/c840/46586/c830/46427/c12771/45951")

  perc_agricultor_ens_bas <- sidrar::get_sidra(api = api_req) |>
    janitor::clean_names() |>
    tibble::tibble() |>
    dplyr::select(
      municipio_codigo,
      municipio,
      ano,
      valor,
      escolaridade_do_produtor
    ) |>
    tidyr::pivot_wider(
      names_from = escolaridade_do_produtor,
      values_from = valor
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ tidyr::replace_na(.x, 0)
      )
    ) |>
    dplyr::mutate(
      agricultor_com_ens_bas = antigo_primario_elementar + antigo_ginasial_medio_1o_ciclo +
        regular_do_ensino_fundamental_ou_1o_grau + eja_educacao_de_jovens_e_adultos_e_supletivo_do_ensino_fundamental_ou_do_1o_grau,
      perc_agricultor_ens_bas = agricultor_com_ens_bas/total *100
    )

  return(perc_agricultor_ens_bas)
}
