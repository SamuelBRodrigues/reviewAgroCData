
#' Coleta completa dos Indicadores de Educação Municipal
#'
#' Executa o pipeline de extração dos indicadores educacionais nos municípios.
#'
#' @param ano Ano de referência. Padrão: "2023"
#' @param download Lógico. Se TRUE, baixa os dados. Padrão: TRUE
#' @param cod_ibge Vetor com códigos IBGE. Padrão: target_cities$municipio_codigo
#'
#' @return Uma lista com os dataframes: ideb_iniciais, ideb_finais, tdi, abandono,
#' matricula_creche, gastos_educacao.
#' @export
pega_dados_indicador_educacao <- function(ano = "2023", download = TRUE, cod_ibge = target_cities$municipio_codigo) {
  ideb_iniciais <- get_IDEB_fundamental(download = download, ano = ano, tipo = "anos_iniciais")
  ideb_finais <- get_IDEB_fundamental(download = download, ano = ano, tipo = "anos_finais")
  tdi <- get_taxa_distorcao_idade_serie(download = download, ano = ano)
  abandono <- get_taxa_abandono_escolar(download = download, ano = ano)
  matricula_creche <- get_taxa_liq_matricula_creche(ano = ano)
  gastos_educacao <- get_gastos_per_capita_educacao(cod_ibge = cod_ibge)

  return(list(
    ideb_iniciais = ideb_iniciais,
    ideb_finais = ideb_finais,
    tdi = tdi,
    abandono = abandono,
    matricula_creche = matricula_creche,
    gastos_educacao = gastos_educacao
  ))
}
