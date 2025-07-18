#' Extrai os dados de Estabelecimentos Agropecuários com Pastagens em Boas condições
#'
#' A função extrai  dados de Estabelecimentos Agropecuários com Pastagens em Boas
#' condições a nivel municipal do Senso Agro de 2017 através da API do SIDRA IBGE.
#'
#' @param cod_ibge Um vetor de caracteres com o código IBGE do município ou "Brasil" para todos os municípios ou "Targets" para os municípios alvo.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_perc_pastagens_boas_cond()
#' }
get_perc_pastagens_boas_cond <- function(cod_ibge = "Brasil"){

  if(cod_ibge == "Brasil"){
    api_req <- "/t/6883/n6/all/v/9587/p/all/c829/46302/c222/110087,113469/c12564/41145/c12440/110016"
  }else if(cod_ibge == "Targets"){
    vetor_municipios_alvo <- "4300406,5107875,5107925,2928901,5103502,5102637,5218805,2911105,5102678,5106224,5211909,5106240,5206206,4306601,5107065,5102686,5107040,2903201,5105259,2909307,2101400,2112001,5006606,5005400,3170404,5102702,2919553,5107958,3170107,5108006,5101902,1508126,5104526,5106307,5007901,5008305,5104609,5007208,2201150,5007109,2708105,4104808,2700300,3147006,2211209,5003256,5213756,4322202,5003702,1708205,5107859"
    api_req <- stringr::str_glue("/t/6883/n6/{vetor_municipios_alvo}/v/9587/p/all/c829/46302/c222/110087,113469/c12564/41145/c12440/110016")
  }else{
    vetor_municipios_alvo <- paste0(cod_ibge, collapse = ",")
    api_req <- stringr::str_glue("/t/6883/n6/{vetor_municipios_alvo}/v/9587/p/all/c829/46302/c222/110087,113469/c12564/41145/c12440/110016")
  }

  perc_pastagens_boas_cond <- sidrar::get_sidra(api = api_req) |>
    janitor::clean_names() |>
    tibble::tibble() |>
    dplyr::select(
      municipio_codigo,
      municipio,
      ano,
      valor,
      utilizacao_das_terras
    ) |>
    tidyr::pivot_wider(
      names_from = utilizacao_das_terras,
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
      perc_pastagens_plantadas_em_boas_condicoes  = pastagens_plantadas_em_boas_condicoes/total * 100
    )

  return(perc_pastagens_boas_cond)
}
