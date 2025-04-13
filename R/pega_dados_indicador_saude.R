
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
pega_dados_indicador_saude <- function(cod_ibge = target_cities$municipio_codigo, ano = "last") {

  if(ano == "last"){
    cobertura_aps <- get_cobertura_aps(ano = 2024)
    cobertura_ab <- get_cobertura_ab(ano = 2020)
    gastos_per_capta <- get_gastos_per_capta_saude(cod_ibge)
    mortalidade_infantil <- get_taxa_mortalidade_infantil(ano = "2023")
    obitos_evitaveis <- get_obitos_evitaveis(ano = "2023")
    abastecimento_esgoto <- get_abastecimento_esgoto()
    cobertura_vacinal <- get_cobertura_vacinal(cod_ibge = cod_ibge)
    equip_por_estab <- get_equip_estab(ano = "25", mes = "02")
    internacoes <- get_internacoes_DATASUS(ano = "25", mes = "01")
    obitos <- get_obitos_DATASUS(ano = "25", mes = "01")
    subnutricao <- constroi_subnutricao_sisvan()
    obesidade <- constroi_obesid_mun()
  } else{
    cobertura_aps <- get_cobertura_aps(ano = ano)
    cobertura_ab <- get_cobertura_ab(ano = ano)
    gastos_per_capta <- get_gastos_per_capta_saude(cod_ibge)
    mortalidade_infantil <- get_taxa_mortalidade_infantil(ano = ano)
    obitos_evitaveis <- get_obitos_evitaveis(ano = ano)
    abastecimento_esgoto <- get_abastecimento_esgoto()
    cobertura_vacinal <- get_cobertura_vacinal(cod_ibge = cod_ibge)
    ano_datasus <- stringr::str_extract(ano, "..$")
    equip_por_estab <- get_equip_estab(ano = ano_datasus, mes = "02")
    internacoes <- get_internacoes_DATASUS(ano = ano_datasus, mes = "01")
    obitos <- get_obitos_DATASUS(ano = ano_datasus, mes = "01")
    subnutricao <- constroi_subnutricao_sisvan()
    obesidade <- constroi_obesid_mun()
  }

  data <- unite_indicador_saude(
    cobertura_aps = cobertura_aps,
    cobertura_ab = cobertura_ab,
    gastos_per_capta = gastos_per_capta,
    mortalidade_infantil = mortalidade_infantil,
    obitos_evitaveis = obitos_evitaveis,
    abastecimento_esgoto = abastecimento_esgoto,
    cobertura_vacinal = cobertura_vacinal,
    equip_por_estab = equip_por_estab,
    internacoes = internacoes,
    obitos = obitos,
    subnutricao = subnutricao,
    obesidade = obesidade
  )
  return(data)
}
