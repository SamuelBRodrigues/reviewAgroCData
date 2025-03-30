#' Obtém dados de Taxa de Distorção Idade-Série por município
#'
#' Esta função baixa e processa dados do INEP sobre Taxa de Distorção Idade-Série
#' (TDI) para municípios brasileiros, filtrando apenas os municípios de interesse.
#'
#' @param download Lógico. Se TRUE (padrão), baixa os dados do INEP mesmo que o arquivo
#'                 já exista localmente. Se FALSE, tenta carregar os dados localmente.
#' @param ano String ou numérico. Ano dos dados desejados (disponíveis de 2006 a 2023).
#'            Padrão é "2023".
#'
#' @return Um data frame contendo:
#' \describe{
#'   \item{municipio_codigo}{Código IBGE do município}
#'   \item{municipio_nome}{Nome do município}
#'   \item{estado_sigla}{Sigla da UF}
#'   \item{tdi_ensino_fundamental[ano]}{Taxa de distorção idade-série no ensino fundamental}
#'   \item{tdi_ensino_medio[ano]}{Taxa de distorção idade-série no ensino médio}
#' }
#'
#' @examples
#' \dontrun{
#' # Baixar dados de 2023 (padrão)
#' tdi_2023 <- get_taxa_distorcao_idade_serie()
#'
#' # Carregar dados de 2020 localmente (se existirem)
#' tdi_2020 <- get_taxa_distorcao_idade_serie(download = FALSE, ano = 2020)
#' }
#'
#' @export
#' @importFrom utils download.file unzip
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter select rename %>%
get_taxa_distorcao_idade_serie <- function(download = TRUE, ano = "2023"){
  # Anos disponíveis
  anos_disponiveis <- as.character(2006:2023)

  # Garantir que o ano seja tratado como string
  ano <- as.character(ano)

  # Verificar se o ano fornecido é válido
  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ", paste(anos_disponiveis, collapse = ", "))
  }

  file_path = paste0('data_raw/TDI_', ano, '_MUNICIPIOS/TDI_MUNICIPIOS_', ano,'.xlsx')

  if (download || !file.exists(file_path)){

    urls <- list(
      "2023" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2023/TDI_2023_MUNICIPIOS.zip",
      "2022" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2022/TDI_2022_MUNICIPIOS.zip",
      "2021" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2021/TDI_2021_MUNICIPIOS.zip",
      "2020" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2020/TDI_2020_MUNICIPIOS.zip",
      "2019" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2019/TDI_2019_MUNICIPIOS.zip",
      "2018" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2018/TDI_2018_MUNICIPIOS.zip",
      "2017" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2017/TDI_2017_MUNICIPIOS.zip",
      "2016" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2016/TDI_2016_MUNICIPIOS.zip",
      "2015" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2015/distorcao_idade_serie/tdi_municipios_2015.zip",
      "2014" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2014/distorcao_idade_serie/tdi_municipios_2014.zip",
      "2013" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2013/distorcao_idade_serie/tdi_municipios_2013.zip",
      "2012" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2012/distorcao_idade_serie/tdi_municipios_2012.zip",
      "2011" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2011/distorcao_idade_serie/tdi_municipios_2011.zip",
      "2010" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_distorcao_idade_serie/2010/dados_tdi_municipios_2010.zip",
      "2009" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_distorcao_idade_serie/2009/dados_tdi_municipios_2009.zip",
      "2008" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_distorcao_idade_serie/2008/tdi_municipios_2008.zip",
      "2007" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_distorcao_idade_serie/2007/tdi_municipios_2007.zip",
      "2006" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_distorcao_idade_serie/2006/tdi_municipios_2006.zip"
    )

    url <- urls[[ano]]
    dir_path = "data_raw/"

    # Ensure the directory exists
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }

    # Download the file
    path <- .download_file(url, dir_path)

    # Extract the ZIP file
    .extract_zip(path, dir_path)

    # Remove the ZIP file after extraction
    .remove_zip(path)
  }

  # Start message for file loading
  message('Loading INEP data from file: ', file_path)

  # Read data, skipping the first 8 rows to avoid unnecessary headers
  inep_data <- read_xlsx(
    file_path,
    col_names = TRUE,
    skip = 7,
    na = paste0('Fonte: Censo da Educação Básica ', ano, '/INEP.')
  )
  message('File loaded successfully.')

  # Load target cities data from the package
  message('Loading target cities data...')
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))
  message('Target cities data loaded.')

  # Filter and select relevant columns from INEP data
  message('Filtering and selecting relevant columns...')
  taxa_de_distorcao <- inep_data %>%
    dplyr::filter(NO_CATEGORIA == "Total" & NO_DEPENDENCIA == "Total") %>%
    dplyr::select(CO_MUNICIPIO,
                  NO_MUNICIPIO,
                  SG_UF,
                  `FUN_CAT_0`,
                  `MED_CAT_0`) %>%
    dplyr::rename(`municipio_codigo` = CO_MUNICIPIO,
                  `municipio_nome` =  NO_MUNICIPIO,
                  `estado_sigla` = SG_UF,
                  !!paste0('tdi_ensino_fundamental_', ano) := `FUN_CAT_0`,
                  !!paste0("tdi_ensino_medio_", ano):= `MED_CAT_0`)

  # Filter distortion rate data to keep only rows for target cities
  message('Filtering data for target cities...')
  taxa_de_distorcao <- taxa_de_distorcao %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Return the filtered data
  message('Data filtered and processed successfully.')
  return(taxa_de_distorcao)
}
