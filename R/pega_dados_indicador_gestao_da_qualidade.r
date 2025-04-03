
#' Coleta completa dos Indicadores de Gestão da Qulidade
#'
#' Executa o pipeline de extração dos indicadores de gestão da qualidade nos municípios.
#'
#' @param ano Ano de referência. Padrão: "2023"
#' @param download Lógico. Se TRUE, baixa os dados. Padrão: TRUE
#'
#' @return Uma lista com os dataframes: endividamento, liquidez, poupanca_corrente.
#' @export
pega_dados_indicador_gestao_qualidade <- function(ano = "2023", download = TRUE) {

  endividamento <- get_endividamento(download=download, ano = ano)
  poupanca_corrente <- get_poupanca_corrente(download=FALSE, ano = ano)
  liquidez <- get_liquidez(download=FALSE, ano = ano)

  return(list(
    endividamento = endividamento,
    poupanca_corrente = poupanca_corrente,
    liquidez = liquidez
  ))
}
