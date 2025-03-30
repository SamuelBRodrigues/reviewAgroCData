
#' Coleta completa dos Indicadores de Proteção Social
#'
#' Executa o pipeline de extração dos indicadores de proteção social nos municípios.
#'
#' @param cod_municipios_ibge Vetor com códigos IBGE dos municípios. Padrão: target_cities$municipio_codigo
#' @param ano Ano de referência. Padrão: "2023"
#' @param mes Mês de referência. Padrão: "06"
#' @param dir Diretório dos dados CRAS. Padrão: "data_raw"
#'
#' @return Uma lista com os dataframes: cad_bf, situacao_pobreza, cras.
#' @export
pega_dados_indicador_protecao_social <- function(cod_municipios_ibge = target_cities$municipio_codigo,
                                                  ano = "2023",
                                                  mes = "06",
                                                  dir = "data_raw") {
  cad_bf <- get_cad_bf_data(cod_municipios_ibge, ano, mes)
  situacao_pobreza <- get_situacao_pobreza_data(cod_municipios_ibge)
  download_cras_data(dir = dir, ano = ano)
  cras <- extract_cras_data(dir = dir, ano = ano)

  return(list(
    cad_bf = cad_bf,
    situacao_pobreza = situacao_pobreza,
    cras = cras
  ))
}
