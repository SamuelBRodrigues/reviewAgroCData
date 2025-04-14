#' Obter Taxa Líquida de Matrícula em Creches por Município
#'
#' Esta função permite obter os dados da taxa líquida de matrícula em creches fornecidos pelo INEP para o ano especificado.
#' Os dados são carregados a partir de um arquivo local.
#'
#' @param ano Um valor numérico ou caractere indicando o ano para o qual os dados devem ser obtidos. Os anos disponíveis
#'   são de 2010 até 2023. O padrão é "2023".
#'
#' @return Um data frame contendo os dados da taxa líquida de matrícula em creches, com as colunas: `municipio_codigo`,
#'   `municipio_nome`, `taxa_liquida_matricula_creche_<ano>`, onde `<ano>` é o ano solicitado.
#'
#' @examples
#' \dontrun{
#'   # Obter taxa líquida de matrícula em creches para o ano de 2023
#'   taxa_2023 <- get_taxa_matricula_creche("2023")
#'
#'   # Obter taxa líquida de matrícula em creches para o ano de 2022
#'   taxa_2022 <- get_taxa_matricula_creche("2022")
#' }
#'
#' @export
get_taxa_liq_matricula_creche <- function(ano = '2023'){
  # Definir os anos disponíveis
  anos_disponiveis <- as.character(2010:2023)

  # Garantir que o ano seja tratado como string
  ano <- as.character(ano)

  # Verificar se o ano fornecido é válido
  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ", paste(anos_disponiveis, collapse = ", "))
  }
  # Filtrar e selecionar as colunas relevantes dos dados do INEP
  message('Filtrando e selecionando colunas relevantes...')
  taxa_liq_matricula <- taxa_matricula %>%
    dplyr::select(`Código IBGE`,
                  `Localidade`,
                  !!paste0('​',ano)) %>%
    dplyr::rename(`municipio_codigo` = `Código IBGE`,
                  `municipio_nome` =  Localidade,
                  !!paste0('taxa_liquida_matricula_creche_', ano) := !!paste0('​',ano))

  # Filtrar os dados para manter apenas as cidades-alvo
  message('Filtrando dados para as cidades-alvo...')
  taxa_liq_matricula <- taxa_liq_matricula %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Retornar os dados filtrados
  message('Dados filtrados e processados com sucesso.')
  return(taxa_liq_matricula)
}
