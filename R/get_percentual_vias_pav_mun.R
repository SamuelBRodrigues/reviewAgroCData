#' Obtém o percentual de vias pavimentadas nos municípios selecionados
#'
#' Esta função coleta dados da Pesquisa Urbanística do Entorno dos Domicílios via API do SIDRA/IBGE
#' e calcula o percentual de domicílios localizados em setores com vias pavimentadas.
#'
#' @return Um tibble com o percentual de domicílios em vias pavimentadas por município e ano.
#' @examples
#' \dontrun{
#'  percentual_vias_pav_mun_data <- get_percentual_vias_pav_mun()
#' }
#' @export
get_percentual_vias_pav_mun <- function(){
  vetor_municipios_alvo <- "4300406,5107875,5107925,2928901,5103502,5102637,5218805,2911105,5102678,5106224,5211909,5106240,5206206,4306601,5107065,5102686,5107040,2903201,5105259,2909307,2101400,2112001,5006606,5005400,3170404,5102702,2919553,5107958,3170107,5108006,5101902,1508126,5104526,5106307,5007901,5008305,5104609,5007208,2201150,5007109,2708105,4104808,2700300,3147006,2211209,5003256,5213756,4322202,5003702,1708205,5107859"
  api_req <- stringr::str_glue("/t/9585/n6/{vetor_municipios_alvo}/v/allxp/p/all/c11275/91176/c14/200,72246")

  pavimentacao <- sidrar::get_sidra(api = api_req) |>
    janitor::clean_names() |>
    tibble::tibble() |>
    dplyr::mutate(variavel = stringr::str_glue("{variavel} - {caracteristicas_do_entorno}")) |>
    dplyr::select(municipio_codigo, municipio, ano, variavel, valor) |>
    tidyr::pivot_wider(names_from = variavel, values_from = valor) |>
    janitor::clean_names() |>
    dplyr::rename(
      domicilios_particulares_permanentes_ocupados_em_setores_censitarios_selecionados_para_a_pesquisa_urbanistica_do_entorno_dos_domicilios_via_pavimentada_existe_2022 = domicilios_particulares_permanentes_ocupados_em_setores_censitarios_selecionados_para_a_pesquisa_urbanistica_do_entorno_dos_domicilios_via_pavimentada_existe,
      domicilios_particulares_permanentes_ocupados_em_setores_censitarios_selecionados_para_a_pesquisa_urbanistica_do_entorno_dos_domicilios_total_2022 = domicilios_particulares_permanentes_ocupados_em_setores_censitarios_selecionados_para_a_pesquisa_urbanistica_do_entorno_dos_domicilios_total
    ) |>
    dplyr::mutate(
      percent_entorno_dos_domicilios_via_pavimentada_2022 =
        domicilios_particulares_permanentes_ocupados_em_setores_censitarios_selecionados_para_a_pesquisa_urbanistica_do_entorno_dos_domicilios_via_pavimentada_existe_2022 /
        domicilios_particulares_permanentes_ocupados_em_setores_censitarios_selecionados_para_a_pesquisa_urbanistica_do_entorno_dos_domicilios_total_2022 * 100
    ) |>
    dplyr::select(
      municipio_codigo,
      percent_entorno_dos_domicilios_via_pavimentada_2022
    )

  return(pavimentacao)
}
