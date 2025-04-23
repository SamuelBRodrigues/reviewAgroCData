#' Coleta completa dos Indicadores de Infraestrutura
#'
#' Executa o pipeline de extração dos indicadores de infraestrutura nos municípios.
#'
#' @param dir Diretorio onde os dados necessários serão baixados e lidos.
#' @param ano Ano de interesse dos dados. Por padrão usa "last" que refere-se ao
#' ano mais recente disponível dos dados
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   dados_indicador_infraestrutura <- pega_dados_indicador_infraestrutura(
#'     dir = "data_agro",
#'     ano = "2024"
#'   )
#' }
pega_dados_indicador_infraestrutura <- function(dir = "data_raw", ano = "last"){

  if(ano == "last"){

    acess_agua_mun <- get_percentual_acess_agua_mun()
    acess_esgot_mun <- get_percentual_acess_esgot_mun()
    vias_pav_mun <- get_percentual_vias_pav_mun()
    cob_inte_mun <- get_internet_data(dir = dir, ano = 2024)
    conect_rural_mun <- get_conectividade_rural()

  } else{

    acess_agua_mun <- get_percentual_acess_agua_mun()
    acess_esgot_mun <- get_percentual_acess_esgot_mun()
    vias_pav_mun <- get_percentual_vias_pav_mun()
    cob_inte_mun <- get_internet_data(dir = dir, ano = ano)
    conect_rural_mun <- get_conectividade_rural()

  }

  data <- unite_indicador_infraestrutura(
    acess_agua_mun = acess_agua_mun,
    acess_esgot_mun = acess_esgot_mun,
    vias_pav_mun = vias_pav_mun,
    cob_inte_mun = cob_inte_mun,
    conect_rural_mun = conect_rural_mun
  )

  return(data)

}
