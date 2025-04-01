#' Obtém dados da Despesa Corrente/Receita Corrente Ajustada (Poupança Corrente) para um ano específico
#'
#' Esta função carrega e processa os dados da poupança corrente para os municípios brasileiros,
#' a partir de arquivos CAPAG disponíveis para os anos de 2017 a 2023. A função permite o
#' download automático dos dados caso o arquivo não esteja presente no diretório especificado.
#'
#' @param download Lógico. Se \code{TRUE}, a função tentará fazer o download dos dados se o arquivo
#'    não estiver presente localmente. O padrão é \code{TRUE}.
#' @param ano Um número ou caractere. O ano para o qual os dados devem ser obtidos. O valor padrão é
#'    2023, mas os anos válidos são de 2017 a 2023.
#'
#' @return Um \code{data.frame} contendo os dados da poupança corrente dos municípios alvo,
#'    com as colunas:
#'    \itemize{
#'      \item \code{municipio_codigo}: Código do município.
#'      \item \code{municipio_nome}: Nome do município.
#'      \item \code{estado_sigla}: Sigla do estado do município.
#'      \item \code{poupanca_corrente_<ano>}: O valor da poupança corrente para o ano especificado.
#'    }
#'
#' @details O arquivo de dados CAPAG utilizado para cada ano é obtido automaticamente a partir de um
#'    diretório local. Caso o arquivo não exista, a função faz o download do mesmo. Para o ano de 2023,
#'    um ajuste no cabeçalho da planilha é feito (saltando 2 linhas de dados).
#'
#' @examples
#' \dontrun{
#' # Obter dados para o ano de 2021
#' poupanca_corrente_2021 <- get_poupanca_corrente(ano = 2021)
#'
#' # Obter dados para o ano de 2023 e sem download
#' poupanca_corrente_2023 <- get_poupanca_corrente(download = FALSE, ano = 2023)
#' }
#'
#' @import dplyr
#' @import readxl
#' @export
get_poupanca_corrente <- function(download = TRUE, ano = 2023) {
  anos_disponiveis <- as.character(2017:2023)
  ano <- as.character(ano)

  # Validação do ano
  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ", paste(anos_disponiveis, collapse = ", "))
  }

  # Definindo o nome do arquivo para cada ano
  file_name <- list(
    '2017' = 'CAPAG-Municipios---2018.xlsx',
    '2018' = 'CAPAG-2019---Municipios.xlsx',
    '2019' = 'CAPAG-Municipios.xlsx',
    '2020' = 'Capag-Municipios---novembro-2021.xlsx',
    '2021' = 'CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx',
    '2022' = 'CAPAG-Municipios-2023.xlsx',
    '2023' = '20241015CAPAG-Municipios.xlsx'
  )

  file_path = paste0('data_raw/CAPAG/', file_name[[ano]])

  if (download || !file.exists(file_path)){
    download_CAPAG(dir_path = 'data_raw/CAPAG/', ano = ano)
  }

  message('Loading CAPAG data from file: ', file_path)

  capag_data <- read_xlsx(
    file_path,
    skip = if (ano == "2023") 2 else 0,
    col_names = TRUE
  )
  message('File loaded successfully.')

  message('Loading target cities data...')
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))
  message('Target cities data loaded.')

  message('Filtering and selecting relevant columns...')
  if (ano == '2021'){
    poupanca_corrente_data <- capag_data %>%
      filter(Ano_Base == '2021') %>%
      mutate(poupanca_corrente = coalesce(as.numeric(Indicador_2_Revisão), as.numeric(Indicador_2))) %>%
      select(`Cod.IBGE`, `poupanca_corrente`) %>%
      rename(`municipio_codigo` = `Cod.IBGE`,
             !!paste0('poupanca_corrente_', ano) := `poupanca_corrente`)
  } else if (ano == '2023'){
    poupanca_corrente_data <- capag_data %>%
      select(`Código Município Completo`, `Indicador 2`) %>%
      rename(`municipio_codigo` = `Código Município Completo`,
             !!paste0('poupanca_corrente_', ano) := `Indicador 2`)
  } else if (ano == '2018'){
    poupanca_corrente_data <- capag_data %>%
      select(`Cod.IBGE`, `Indicador 2`) %>%
      rename(`municipio_codigo` = `Cod.IBGE`,
             !!paste0('poupanca_corrente_', ano) := `Indicador 2`)
  } else {
    poupanca_corrente_data <- capag_data %>%
      select(`Cod.IBGE`, `Indicador_2`) %>%
      rename(`municipio_codigo` = `Cod.IBGE`,
             !!paste0('poupanca_corrente_', ano) := `Indicador_2`)
  }

  message('Filtering data for target cities...')
  poupanca_corrente_data <- poupanca_corrente_data %>%
    mutate(municipio_codigo = as.character(municipio_codigo)) %>%
    right_join(target_cities, by = 'municipio_codigo') %>%
    select(municipio_codigo, municipio_nome, estado_sigla, !!paste0('poupanca_corrente_', ano))

  message('Data filtered and processed successfully.')
  return(poupanca_corrente_data)
}
