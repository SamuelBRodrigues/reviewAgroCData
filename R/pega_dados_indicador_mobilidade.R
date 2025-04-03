
#' Coleta completa dos Indicadores de Mobilidade Urbana
#'
#' Executa o pipeline de extração dos indicadores de mobilidade urbana nos municípios.
#'
#' @param ano Ano dos dados. Padrão: "last"
#'
#' @return Uma lista com os dataframes: internacoes_transito, obitos_transito, deslocamento_sidra, emissao.
#' @export
pega_dados_indicador_mobilidade <- function(ano = "last") {

  if(ano == "last"){

    internacoes_transito <- get_internacoes_DATASUS(ano = "25", mes = "01")
    obitos_transito <- get_obitos_DATASUS(ano = "25", mes = "01")
    deslocamento_sidra <- get_deslocamento_SIDRA()
    emissao <- get_emissao()

  } else{
    internacoes_transito <- get_internacoes_DATASUS(ano = ano, mes = "01")
    obitos_transito <- get_obitos_DATASUS(ano = ano, mes = "01")
    deslocamento_sidra <- get_deslocamento_SIDRA()
    emissao <- get_emissao()
  }

  data <- unite_indicador_mobilidade(
    internacoes_transito = internacoes_transito,
    obitos_transito = obitos_transito,
    deslocamento_sidra = deslocamento_sidra,
    emissao = emissao
  )

  return(data)
}
