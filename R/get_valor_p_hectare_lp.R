#' Extrai os dados do Valor da Produção das Lavouras Permanentes por Hectare
#'
#' A função extrai  dados Valor da Produção das Lavouras Permanentes por Hectare
#' do Senso Agro de 2017 através da API do SIDRA IBGE.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_valor_p_hectare_lp()
#' }
get_valor_p_hectare_lp <- function(){

  vetor_municipios_alvo <- "5107875,5107925,2928901,5103502,5102637,5218805,2911105,5102678,5106224,5211909,5106240,5206206,4306601,5107065,5102686,5107040,2903201,5105259,2909307,2101400,2112001,5006606,5005400,3170404,5102702,2919553,5107958,3170107,5108006,5101902,1508126,5104526,5106307,5007901,5008305,5104609,5007208,2201150,5007109,2708105,4104808,2700300,3147006,2211209,5003256,5213756,4322202,5003702,1708205,5107859"

  # /t/6956/n6/2700300/v/10076,10079/p/all/c829/46302/c227/113864/c220/110085/d/v10076%200,v10079%200
  api_req <- stringr::str_glue(
    "/t/6956/n6/{vetor_municipios_alvo}/v/10076,10079/p/all/c829/46302/c227/113864/c220/110085/d/v10076%200,v10079%200")

  get_valor_p_hectare_lp <- sidrar::get_sidra(api = api_req) |>
    janitor::clean_names() |>
    tibble::tibble() |>
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
      valor_da_producao_das_lavouras_permanentes = valor_da_producao_das_lavouras_permanentes_nos_estabelecimentos_agropecuarios_com_50_pes_e_mais_existentes * 1000,
      prod_por_hec_lavouras_permanentes = valor_da_producao_das_lavouras_permanentes/area_total_existente_na_data_de_referencia_nas_lavouras_permanentes_nos_estabelecimentos_agropecuarios_com_50_pes_e_mais_existentes
    )

  return(get_valor_p_hectare_lp)

}
