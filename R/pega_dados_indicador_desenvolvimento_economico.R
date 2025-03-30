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
pega_dados_indicador_desenvolvimento_economico <- function(ano_pib = 2021) {
  # Carrega dados de PIB per capita
  pib <- get_PIB_per_capta(ano = ano_pib)

  # Carrega dados de empregos formais por 100k habitantes
  empregos <- get_empregos_formais_por_100khab()

  # Carrega dados do IVS 2010
  ivs <- extract_IVS()

  # Junta os dados
  indicadores_desenvolvimento <- empregos %>%
    dplyr::left_join(pib, by = c("municipio_codigo", "municipio_nome", "estado_sigla")) %>%
    dplyr::left_join(ivs, by = c("municipio_nome", "estado_sigla"))

  return(indicadores_desenvolvimento)
}
