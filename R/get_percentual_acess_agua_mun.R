#' Obtém o percentual de domicílios com acesso à rede geral de água
#'
#' A função consulta a API do SIDRA/IBGE para obter dados sobre abastecimento de água e calcula
#' o percentual de domicílios com ligação à rede geral como forma principal de abastecimento.
#'
#' @param cod_ibge Um vetor de caracteres com o código IBGE do município ou "Brasil" para todos os municípios ou "Targets" para os municípios alvo.
#'
#' @return Um tibble com o percentual de domicílios com acesso à rede geral de água por município e ano.
#' @examples
#' \dontrun{
#'  percentual_acess_agua_mun_data <- get_percentual_acess_agua_mun()
#' }
#'
#' @export
get_percentual_acess_agua_mun <- function(cod_ibge = "Brasil"){

  if(cod_ibge == "Brasil"){
    api_req <- "/t/6751/n6/all/v/9599/p/all/c1821/72129,72144/c14/200"
  }else if(cod_ibge == "Targets"){
    vetor_municipios_alvo <- "4300406,5107875,5107925,2928901,5103502,5102637,5218805,2911105,5102678,5106224,5211909,5106240,5206206,4306601,5107065,5102686,5107040,2903201,5105259,2909307,2101400,2112001,5006606,5005400,3170404,5102702,2919553,5107958,3170107,5108006,5101902,1508126,5104526,5106307,5007901,5008305,5104609,5007208,2201150,5007109,2708105,4104808,2700300,3147006,2211209,5003256,5213756,4322202,5003702,1708205,5107859"
    api_req <- stringr::str_glue("/t/6751/n6/{vetor_municipios_alvo}/v/9599/p/all/c1821/72129,72144/c14/200")
  }else{
    vetor_municipios_alvo <- paste0(cod_ibge, collapse = ",")
    api_req <- stringr::str_glue("/t/6751/n6/{vetor_municipios_alvo}/v/9599/p/all/c1821/72129,72144/c14/200")
  }

  acess_agua_mun <- sidrar::get_sidra(api = api_req) |>
    janitor::clean_names() |>
    tibble::tibble() |>
    dplyr::mutate(variavel = stringr::str_glue("{variavel} - {existencia_de_ligacao_a_rede_geral_de_distribuicao_de_agua_e_principal_forma_de_abastecimento_de_agua}")) |>
    dplyr::select(municipio_codigo, municipio, ano, variavel, valor) |>
    tidyr::pivot_wider(names_from = variavel, values_from = valor) |>
    janitor::clean_names() |>
    dplyr::mutate(
      percent_acess_agua_mun_2022 =
        domicilios_particulares_permanentes_ocupados_em_setores_censitarios_selecionados_para_a_pesquisa_urbanistica_do_entorno_dos_domicilios_possui_ligacao_a_rede_geral_e_a_utiliza_como_forma_principal /
        domicilios_particulares_permanentes_ocupados_em_setores_censitarios_selecionados_para_a_pesquisa_urbanistica_do_entorno_dos_domicilios_total * 100
    ) |>
    dplyr::select(
      municipio_codigo, percent_acess_agua_mun_2022
    )

  return(acess_agua_mun)
}
