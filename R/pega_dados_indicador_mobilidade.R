
#' Coleta completa dos Indicadores de Mobilidade Urbana
#'
#' Executa o pipeline de extração dos indicadores de mobilidade urbana nos municípios.
#'
#' @param ano Ano dos dados. Padrão: "22"
#'
#' @return Uma lista com os dataframes: internacoes_transito, obitos_transito, deslocamento_sidra, emissao.
#' @export
pega_dados_indicador_mobilidade <- function(ano = "22") {
  internacoes_transito <- get_internacoes_DATASUS(ano = ano)
  obitos_transito <- get_obitos_DATASUS(ano = ano)
  deslocamento_sidra <- get_deslocamento_SIDRA()
  emissao <- get_emissao()

  return(list(
    internacoes_transito = internacoes_transito,
    obitos_transito = obitos_transito,
    deslocamento_sidra = deslocamento_sidra,
    emissao = emissao
  ))
}
