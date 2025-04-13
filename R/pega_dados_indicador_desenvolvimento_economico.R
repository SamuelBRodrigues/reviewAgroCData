#' Coleta e organiza os dados da dimensão de Desenvolvimento Econômico
#'
#' Esta função combina os principais indicadores de desenvolvimento econômico:
#' - PIB per capita
#' - Empregos formais por 100 mil habitantes
#' - IVS 2010 (renda e infraestrutura)
#'
#' @param ano_pib Ano para coleta do PIB per capita. Padrão é 2021.
#'
#' @return Um data.frame com os indicadores combinados por município
#' @export
#'
#' @examples
#' \dontrun{
#'   pega_dados_indicador_desenvolvimento_economico(ano_pib = 2021)
#' }
pega_dados_indicador_desenvolvimento_economico <- function(ano= "last") {

  if(ano == "last"){
    pib <- get_PIB_per_capta(ano = 2021)
    empregos <- get_empregos_formais_por_100khab()
    ivs <- extract_IVS()
    investimento_per_capita <- get_investimento_per_capita(ano = "2024")
    recebe_assist <- get_recebe_assist()
    produtividade_agricola_lt <- get_valor_p_hectare_lt()
    produtividade_agricola_lp <- get_valor_p_hectare_lp()
    perc_produtores_jovens <- get_perc_prod_jovens_25_35_anos()
    perc_pessoas_parentesco <- get_perc_laco_parentesco_produtor()
    prop_sistemas_florestais <- get_perc_area_cultv_espec_flor()
    prop_boas_pastagens <- get_perc_pastagens_boas_cond()
    empresas_abertas <- get_perc_empresas_permanecem_abertas()
    perc_estab_agric_familia <- get_percentual_estabelecimentos_agricultura_familiar()
  } else{
    pib <- get_PIB_per_capta(ano = ano)
    empregos <- get_empregos_formais_por_100khab()
    ivs <- extract_IVS()
    investimento_per_capita <- get_investimento_per_capita(ano = ano)
    recebe_assist <- get_recebe_assist()
    produtividade_agricola <- get_valor_p_hectare_lt()
    perc_produtores_jovens <- get_perc_prod_jovens_25_35_anos()
    perc_pessoas_parentesco <- get_perc_laco_parentesco_produtor()
    prop_sistemas_florestais <- get_perc_area_cultv_espec_flor()
    prop_boas_pastagens <- get_perc_pastagens_boas_cond()
    empresas_abertas <- get_perc_empresas_permanecem_abertas()
    perc_estab_agric_familia <- get_percentual_estabelecimentos_agricultura_familiar()
  }
  data <- unite_indicador_desenvolvimento_economico(
    pib = pib,
    empregos = empregos,
    ivs = ivs,
    investimento_per_capita = investimento_per_capita,
    recebe_assist = recebe_assist,
    produtividade_agricola_lt = produtividade_agricola_lt,
    produtividade_agricola_lp = produtividade_agricola_lp,
    perc_produtores_jovens = perc_produtores_jovens,
    perc_pessoas_parentesco = perc_pessoas_parentesco,
    prop_sistemas_florestais = prop_sistemas_florestais,
    prop_boas_pastagens = prop_boas_pastagens,
    empresas_abertas = empresas_abertas,
    perc_estab_agric_familia = perc_estab_agric_familia
  )

  return(data)
}
