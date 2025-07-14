#' Obter Taxa de Distorção Idade-Série por Município
#'
#' Esta função permite obter os dados de taxa de distorção idade-série fornecidos pelo INEP para o ano especificado.
#' Os dados podem ser baixados automaticamente, caso não estejam presentes no diretório local.
#'
#' @param download Um valor lógico (TRUE ou FALSE). Se TRUE, a função fará o download dos arquivos caso eles não existam
#'   localmente. Caso contrário, apenas tentará carregar os dados já existentes.
#' @param ano Um valor numérico ou caractere indicando o ano para o qual os dados devem ser obtidos. Os anos disponíveis
#'   são: 2006 até 2023.
#'
#' @return Um data frame contendo os dados de taxa de distorção idade-série, com as colunas: `municipio_codigo`, `municipio_nome`,
#'   `estado_sigla`, `tdi_ensino_fundamental_<ano>`, `tdi_ensino_medio_<ano>`, onde `<ano>` é o ano solicitado.
#'
#' @examples
#' \dontrun{
#'   # Obter taxa de distorção idade-série para o ano de 2023
#'   tdi_2023 <- get_taxa_distorcao_idade_serie(download = TRUE, ano = "2023")
#'
#'   # Obter taxa de distorção idade-série para o ano de 2022 sem baixar o arquivo
#'   tdi_2022 <- get_taxa_distorcao_idade_serie(download = FALSE, ano = "2022")
#' }
#'
#' @export
get_taxa_abandono_escolar <- function(download = TRUE, ano = "2023") {
  # Anos disponíveis
  anos_disponiveis <- as.character(2007:2023)

  # Garantir que o ano seja tratado como string
  ano <- as.character(ano)

  # Verificar se o ano fornecido é válido
  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ", paste(anos_disponiveis, collapse = ", "))
  }

  # Mapeamento dos URLs por ano
  urls <- list(
    "2023" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2023/tx_rend_municipios_2023.zip",
    "2022" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2022/tx_rend_municipios_2022.zip",
    "2021" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2021/tx_rend_municipios_2021.zip",
    "2020" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2020/tx_rend_municipios_2020.zip",
    "2019" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2019/tx_rend_municipios_2019.zip",
    "2018" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2018/TX_REND_MUNICIPIOS_2018.zip",
    "2017" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2017/TAXA_REND_2017_MUNICIPIOS.zip",
    "2016" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2016/TAXA_REND_2016_MUNICIPIOS.zip",
    "2015" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2015/taxa_rendimento/tx_rendimento_municipios_2015.zip",
    "2014" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2014/taxa_rendimento/tx_rendimento_municipios_2014.zip",
    "2013" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2013/taxa_rendimento/tx_rendimento_municipios_2013.zip",
    "2012" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2012/taxas_rendimento/tx_rendimento_municipios_2012.zip",
    "2011" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_rendimento/2011/tx_rendimento_municipios_2011_2.zip",
    "2010" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_rendimento/2010/tx_rendimento_municipios_2010.zip",
    "2009" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_rendimento/2009/tx_rendimento_municipios_2009.zip",
    "2008" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_rendimento/2008/tx_rendimento_municipios_2008.zip",
    "2007" = "https://download.inep.gov.br/informacoes_estatisticas/2011/indicadores_educacionais/taxa_rendimento/2007/tx_rendimento_municipios_2007.zip"
  )

  # Configurar URL e caminho do arquivo
  url <- urls[[ano]]
  file_path <- paste0('data_raw/tx_rend_municipios_', ano, '/tx_rend_municipios_', ano, '.xlsx')

  # Verificar se precisa baixar o arquivo
  if (download || !file.exists(file_path)) {
    dir_path <- 'data_raw/'
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }

    # Baixar o arquivo com tratamento de erro para a conexão SSL
    tryCatch({
      path <- .download_file(url, dir_path)
      .extract_zip(path, dir_path)
      .remove_zip(path)
    }, error = function(e) {
      stop("Erro durante o download ou extração: ", e$message)
    })
  }

  # Listar arquivos extraídos
  extracted_files <- list.files(path = dirname(file_path), full.names = TRUE)
  message("Arquivos extraídos: ", paste(extracted_files, collapse = ", "))

  # Procurar pelo arquivo .xlsx
  xlsx_file <- grep("\\.xlsx$", extracted_files, value = TRUE)

  if (length(xlsx_file) == 0) {
    stop("Arquivo Excel não encontrado na pasta extraída.")
  }

  message("Arquivo Excel encontrado: ", xlsx_file)

  # Carregar os dados
  message('Carregando dados do INEP a partir do arquivo: ', xlsx_file)
  inep_data <- readxl::read_xlsx(
    xlsx_file,
    col_names = TRUE,
    skip = 8,
    na = c("NA")
  )
  message('Arquivo carregado com sucesso.')

  # Selecionar e filtrar os dados
  message('Filtrando e selecionando as colunas relevantes...')
  taxa_abandono <- inep_data %>%
    dplyr::filter(NO_CATEGORIA == "Total" & NO_DEPENDENCIA == "Total") %>%
    dplyr::select(CO_MUNICIPIO, NO_MUNICIPIO, SG_UF, `3_CAT_FUN`, `3_CAT_MED`) %>%
    dplyr::rename(
      municipio_codigo = CO_MUNICIPIO,
      municipio_nome = NO_MUNICIPIO,
      estado_sigla = SG_UF,
      !!paste0("taxa_abandono_fundamental_", ano) := `3_CAT_FUN`,
      !!paste0("taxa_abandono_medio_", ano) := `3_CAT_MED`
    )

  # Converter colunas de taxa de abandono para numérico e lidar com valores não numéricos
  taxa_abandono <- taxa_abandono %>%
    dplyr::mutate(
      dplyr::across(starts_with("taxa_abandono"), ~ as.numeric(gsub(",", ".", ., fixed = TRUE))),
      dplyr::across(starts_with("taxa_abandono"), ~ ifelse(is.na(.), 0, .))  # Substituir NA por 0
    )

  message('Dados filtrados e processados com sucesso.')
  return(taxa_abandono)
}
