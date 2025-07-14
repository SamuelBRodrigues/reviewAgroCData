#' Extração dos dados de Cobertura de Atenção Primária a Saúde (APS)
#'
#' A função extrai os dados de Cobertura APS diretamente da plataforma e-Gestor
#' do ministério da Saúde. Os dados são detalhados a nível municipal para o ano
#' e mês inserido
#'
#' @param ano Ano de interesse dos dados. Por padrão usa o ano de 2024
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   dados_cobertura_aps <- get_cobertura_aps(
#'     ano = "2024",
#'     mes = "01"
#'   )
#' }
get_cobertura_aps <- function(ano = 2024){

  if(ano == 2025){
    data <- "https://relatorioaps-prd.saude.gov.br/cobertura/aps?unidadeGeografica=MUNICIPIO&nuCompInicio=202501&nuCompFim=202504"
  } else if(ano == 2024){
    data <- "https://relatorioaps-prd.saude.gov.br/cobertura/aps?unidadeGeografica=MUNICIPIO&nuCompInicio=202401&nuCompFim=202412"
  } else if(ano == 2023){
    data <- "https://relatorioaps-prd.saude.gov.br/cobertura/aps?unidadeGeografica=MUNICIPIO&nuCompInicio=202301&nuCompFim=202312"
  } else if(ano == 2022){
    data <- "https://relatorioaps-prd.saude.gov.br/cobertura/aps?unidadeGeografica=MUNICIPIO&nuCompInicio=202201&nuCompFim=202212"
  } else if(ano == 2021){
    data <- "https://relatorioaps-prd.saude.gov.br/cobertura/aps?unidadeGeografica=MUNICIPIO&nuCompInicio=202101&nuCompFim=202112"
  } else{
    stop("Anos diponíveis apenas para 2021 a 2025")
  }

  dataset <- jsonlite::read_json(data) %>%
    tibble::tibble() %>%
    tidyr::unnest_wider(".") %>%
    janitor::clean_names()
}
