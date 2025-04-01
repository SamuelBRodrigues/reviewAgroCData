
#' Coleta completa dos Indicadores de Saúde
#'
#' Executa o pipeline de extração dos indicadores de saúde nos municípios.
#'
#' @param cod_ibge Vetor com códigos IBGE dos municípios. Padrão: `target_cities$municipio_codigo`.
#' @param ano Ano de interesse. Padrão: "2023".
#'
#' @return Uma lista com os dataframes: gastos_per_capta, mortalidade_infantil, obitos_evitaveis,
#' abastecimento_esgoto, cobertura_aps, cobertura_vacinal.
#' @export
pega_dados_indicador_saude <- function(cod_ibge = target_cities$municipio_codigo, ano = "2023") {
  cobertura_aps <- get_cobertura_aps(ano = ano)
  gastos_per_capta <- get_gastos_per_capta_saude(cod_ibge)
  mortalidade_infantil <- get_taxa_mortalidade_infantil(ano = ano)
  obitos_evitaveis <- get_obitos_evitaveis(ano = ano)
  abastecimento_esgoto <- get_abastecimento_esgoto()
  cobertura_vacinal <- get_cobertura_vacinal(cod_ibge = cod_ibge)

  return(list(
    gastos_per_capta = gastos_per_capta,
    mortalidade_infantil = mortalidade_infantil,
    obitos_evitaveis = obitos_evitaveis,
    abastecimento_esgoto = abastecimento_esgoto,
    cobertura_aps = cobertura_aps,
    cobertura_vacinal = cobertura_vacinal
  ))
}
