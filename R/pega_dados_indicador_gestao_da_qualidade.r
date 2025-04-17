
#' Coleta completa dos Indicadores de Gestão da Qulidade
#'
#' Executa o pipeline de extração dos indicadores de gestão da qualidade nos municípios.
#'
#' @param ano Ano de referência. Padrão: "last" ano mais recente.
#' @param download Lógico. Se TRUE, baixa os dados. Padrão: TRUE
#'
#' @return Uma lista com os dataframes: endividamento, liquidez, poupanca_corrente.
#' @export
pega_dados_indicador_gestao_qualidade <- function(ano = "last", download = TRUE) {

  if(ano == "last"){
    endividamento <- get_endividamento(download=download, ano = "2025")
    poupanca_corrente <- get_poupanca_corrente(download=download, ano = "2023")
    liquidez <- get_liquidez(download=download, ano = "2023")
  } else{
    endividamento <- get_endividamento(download=download, ano = ano)
    poupanca_corrente <- get_poupanca_corrente(download=download, ano = ano)
    liquidez <- get_liquidez(download=download, ano = ano)
  }

  data <- unite_indicador_gestao_qualidade(
    endividamento = endividamento,
    poupanca_corrente = poupanca_corrente,
    liquidez = liquidez
  )

  return(data)
}
