
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
pega_dados_indicador_educacao <- function(ano = "last", download = TRUE, cod_ibge = target_cities$municipio_codigo) {
  if(ano == "last"){
    ideb_iniciais <- get_IDEB_fundamental(download = download, ano = "2023", tipo = "anos_iniciais")
    ideb_finais <- get_IDEB_fundamental(download = download, ano = "2023", tipo = "anos_finais")
    tdi <- get_taxa_distorcao_idade_serie(download = download, ano = "2023")
    abandono <- get_taxa_abandono_escolar(download = download, ano = "2023")
    matricula_creche <- get_taxa_liq_matricula_creche(ano = "2023")
    gastos_educacao <- get_gastos_per_capita_educacao(cod_ibge = cod_ibge)
    gestao <- get_gestao_dr_vs_de(ano = 2023)
    perc_agricultor_ens_bas <- get_perc_agricultor_ens_bas()
    perc_alimentos_pnae <- get_alimentos_pnae(ano = 2022)
  } else{
    ideb_iniciais <- get_IDEB_fundamental(download = download, ano = ano, tipo = "anos_iniciais")
    ideb_finais <- get_IDEB_fundamental(download = download, ano = ano, tipo = "anos_finais")
    tdi <- get_taxa_distorcao_idade_serie(download = download, ano = ano)
    abandono <- get_taxa_abandono_escolar(download = download, ano = ano)
    matricula_creche <- get_taxa_liq_matricula_creche(ano = ano)
    gastos_educacao <- get_gastos_per_capita_educacao(cod_ibge = cod_ibge)
    gestao <- get_gestao_dr_vs_de(ano = ano)
    perc_agricultor_ens_bas <- get_perc_agricultor_ens_bas()
    perc_alimentos_pnae <- get_alimentos_pnae(ano = ano)

  }

  data <- unite_indicador_ed(
    ideb_finais = ideb_finais,
    ideb_iniciais = ideb_iniciais,
    tdi = tdi,
    abandono_escolar = abandono,
    matricula_creche = matricula_creche,
    gastos_educacao = gastos_educacao,
    gestao = gestao,
    perc_agricultor_ens_bas = perc_agricultor_ens_bas,
    perc_alimentos_pnae = perc_alimentos_pnae
  )

  return(data)
}
