#' Obter PIB per capita dos municípios
#'
#' Esta função permite obter o PIB per capita dos municípios de um ano específico.
#' Se o arquivo de dados não estiver disponível, ele será baixado automaticamente.
#'
#' @param download Lógico. Se `TRUE`, a função irá tentar baixar os dados mais recentes
#' caso o arquivo não esteja presente no diretório local. O padrão é `TRUE`.
#'
#' @param ano Ano para o qual o PIB per capita será obtido. O valor padrão é `2021`.
#' Os anos válidos são de 2010 a 2021.
#'
#' @return Um data frame contendo o código do município, a sigla da unidade da federação,
#' o nome do município e o PIB per capita para o ano especificado.
#'
#' @examples
#' \dontrun{
#'   pib_data <- get_PIB_per_capta(ano = 2021)
#'   head(pib_data)
#' }
#'
#' @import dplyr
#' @import readxl
#'
#' @export
get_PIB_per_capta <- function(download = TRUE, ano=2021){

  anos_disponiveis <- as.character(2010:2021)
  ano <- as.character(ano)

  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ",
         paste(anos_disponiveis, collapse = ", "))
  }

  file_path = 'data_raw/PIB dos Munic�pios - base de dados 2010-2021.xlsx'

  if (download || !file.exists(file_path)){
    url <- "https://ftp.ibge.gov.br/Pib_Municipios/2021/base/base_de_dados_2010_2021_xlsx.zip"
    dir_path = "data_raw/"

    # Garantir que o diretório existe
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }

    # Baixar e extrair o arquivo
    path <- .download_file(url, dir_path)
    .extract_zip(path, dir_path)
    .remove_zip(path)
  }

  # Ler os dados mantendo os nomes das colunas
  pib_data <- readxl::read_xlsx(
    file_path,
    col_names = TRUE
  )

  # Carregar cidades alvo
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))

  # Filtrar e selecionar dados
  pib_per_capita <- pib_data %>%
    dplyr::filter(Ano == ano,
                  `Código do Município` %in% target_cities$municipio_codigo) %>%
    dplyr::select(`Código do Município`,
                  `Nome do Município`,
                  `Sigla da Unidade da Federação`,
                  `Produto Interno Bruto per capita, \r\na preços correntes\r\n(R$ 1,00)`
    ) %>%
    rename(municipio_codigo = `Código do Município`,
           municipio_nome = `Nome do Município`,
           estado_sigla = `Sigla da Unidade da Federação`,
           !!paste0('PIB_per_capta_', ano):=`Produto Interno Bruto per capita, \r\na preços correntes\r\n(R$ 1,00)`)

    return(pib_per_capita)
}
