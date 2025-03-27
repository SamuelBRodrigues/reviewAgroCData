#' Obtém os dados do IDEB 2021 para os anos finais do ensino fundamental
#'
#' Esta função obtém os dados do IDEB (Índice de Desenvolvimento da Educação Básica)
#' para os anos finais do ensino fundamental em municípios brasileiros no ano de 2021.
#' Caso necessário, a função realiza o download do conjunto de dados antes do processamento.
#'
#' @param download Lógico. Se `TRUE`, a função baixa o conjunto de dados do IDEB antes do processamento.
#' O padrão é `TRUE`.
#' @param file_path Caracter. Caminho para o arquivo Excel contendo os dados do IDEB.
#' O padrão é `'data_raw/divulgacao_anos_finais_municipios_2023/divulgacao_anos_finais_municipios_2023.xlsx'`.
#'
#' @return Um data frame contendo os dados processados do IDEB para os anos finais do ensino fundamental.
#'
#' @details
#' A função segue os seguintes passos:
#' 1. Se `download = TRUE`, chama `download_IDEB_fundamental()` para baixar e extrair os dados do IDEB.
#' 2. Chama `extract_IDEB_2021_anos_finais()` para processar e retornar os dados filtrados.
#'
#' @examples
#' \dontrun{
#' # Baixar e obter os dados do IDEB para os anos finais
#' ideb_dados <- get_IDEB_2021_anos_finais()
#'
#' # Utilizar um arquivo já existente sem baixar novamente
#' ideb_dados <- get_IDEB_2021_anos_finais(download = FALSE, file_path = "caminho_personalizado.xlsx")
#' }
#'
#' @export

get_IDEB_2021_anos_finais <- function(download = TRUE,
                                      file_path = 'data_raw/divulgacao_anos_finais_municipios_2023/divulgacao_anos_finais_municipios_2023.xlsx'){

  if (download){
    download_IDEB_fundamental(dir_path = "data_raw/")
    file_path = 'data_raw/divulgacao_anos_finais_municipios_2023/divulgacao_anos_finais_municipios_2023.xlsx'
  }

  ideb_2021_anos_finais <- extract_IDEB_2021_anos_finais(file_path = file_path)

  return(ideb_2021_anos_finais)
}
