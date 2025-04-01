#' Obtém dados municipais de Obrigações Financeiras/Disponibilidade de Caixa (Liquidez)
#'
#' Esta função baixa e processa os dados de liquidez municipal a partir dos arquivos CAPAG.
#'
#' @param download Lógico. Se `TRUE`, baixa o arquivo correspondente ao ano escolhido. Default é `TRUE`.
#' @param ano Inteiro ou caractere. Ano desejado para a obtenção dos dados. Valores válidos: 2017 a 2023. Default é 2023.
#'
#' @return Um `data.frame` contendo os dados de liquidez dos municípios para o ano especificado, incluindo os códigos dos municípios, nomes e estados.
#'
#' @examples
#'
#' # Obtendo dados para 2023 (sem baixar novamente se o arquivo já existir)
#' df <- get_liquidez(download = FALSE, ano = 2023)
#'
#' # Obtendo dados para 2021 e assegurando a realização do download
#' df <- get_liquidez(download = TRUE, ano = 2021)
#'
#' @export
get_liquidez <- function(download = TRUE, ano = 2023) {...}

get_liquidez <- function(download = TRUE, ano = 2023){
  anos_disponiveis <- as.character(2017:2023)
  ano <- as.character(ano)

  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ",
         paste(anos_disponiveis, collapse = ", "))
  }

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
    col_names = TRUE,
  )
  message('File loaded successfully.')

  message('Loading target cities data...')
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))
  message('Target cities data loaded.')


  message('Filtering and selecting relevant columns...')
  if (ano == '2021'){
    liquidez_data <- capag_data %>%
      filter(Ano_Base== '2021') %>%
      mutate(liquidez = coalesce(as.numeric(Indicador_3_Revisão), as.numeric(Indicador_3))) %>%
      select(`Cod.IBGE`,
             `liquidez`) %>%
      rename(`municipio_codigo` = `Cod.IBGE`,
             !!paste0('liquidez_',ano) := `liquidez`)

  } else if (ano == '2023'){

    liquidez_data <- capag_data %>%
      select(`Código Município Completo`,
             `Indicador 3`) %>%
      rename(`municipio_codigo` = `Código Município Completo`,
             !!paste0('liquidez_',ano) := `Indicador 3`)
  } else if (ano == '2018'){
    liquidez_data <- capag_data %>%
      select(`Cod.IBGE`,
             `Indicador 3`) %>%
      rename(`municipio_codigo` = `Cod.IBGE`,
             !!paste0('liquidez_',ano) := `Indicador 3`)
  } else{
    liquidez_data <- capag_data %>%
      select(`Cod.IBGE`,
             `Indicador_3`) %>%
      rename(`municipio_codigo` = `Cod.IBGE`,
             !!paste0('liquidez_',ano) := `Indicador_3`)
  }

  message('Filtering data for target cities...')
  liquidez_data <- liquidez_data %>%
    mutate(municipio_codigo = as.character(municipio_codigo)) %>%
    right_join(target_cities, by='municipio_codigo') %>%
    select(municipio_codigo,
           municipio_nome,
           estado_sigla,
           !!paste0('liquidez_', ano))

  message('Data filtered and processed successfully.')
  return(liquidez_data)
}
