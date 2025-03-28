#' Obter Dados do IDEB para o Ensino Fundamental
#'
#' Esta função permite obter os dados do IDEB (Índice de Desenvolvimento da Educação Básica) para o Ensino Fundamental,
#' tanto para os anos finais quanto para os anos iniciais. Os dados podem ser baixados automaticamente, caso não estejam
#' presentes no diretório local.
#'
#' @param download Um valor lógico (TRUE ou FALSE). Se TRUE, a função fará o download dos arquivos caso eles não existam
#'   localmente. Caso contrário, apenas tentará carregar os dados já existentes.
#' @param ano Um valor numérico ou caractere indicando o ano para o qual os dados devem ser obtidos. Os anos disponíveis
#'   são: 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021 e 2023.
#' @param tipo Um valor caractere, podendo ser "anos_finais" ou "anos_iniciais". Define se os dados são dos anos finais
#'   ou iniciais do Ensino Fundamental.
#'
#' @return Um data frame contendo os dados do IDEB filtrados, com as colunas: `municipio_codigo`, `municipio_nome`,
#'   `estado_sigla` e o valor do IDEB para o ano solicitado.
#'
#' @examples
#' \dontrun{
#'   # Obter dados do IDEB para os anos finais de 2023
#'   ideb_finais <- get_IDEB_fundamental(download = TRUE, ano = "2023", tipo = "anos_finais")
#'
#'   # Obter dados do IDEB para os anos iniciais de 2023
#'   ideb_iniciais <- get_IDEB_fundamental(download = TRUE, ano = "2023", tipo = "anos_iniciais")
#' }
#'
#' @export
get_IDEB_fundamental <- function(download = TRUE, ano = "2023", tipo = "anos_finais") {
  # Anos disponíveis
  anos_disponiveis <- c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019", "2021", "2023")

  # Verificar se o ano fornecido é válido
  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ", paste(anos_disponiveis, collapse = ", "))
  }

  # Definir o nome do arquivo com base no tipo (finais ou iniciais)
  if (tipo == "anos_finais") {
    file_path <- "data_raw/divulgacao_anos_finais_municipios_2023/divulgacao_anos_finais_municipios_2023.xlsx"
    url <- "https://download.inep.gov.br/ideb/resultados/divulgacao_anos_finais_municipios_2023.zip"
  } else if (tipo == "anos_iniciais") {
    file_path <- 'data_raw/divulgacao_anos_iniciais_municipios_2023/divulgacao_anos_iniciais_municipios_2023.xlsx'
    url <- "https://download.inep.gov.br/ideb/resultados/divulgacao_anos_iniciais_municipios_2023.zip"
  } else {
    stop("Tipo inválido. Escolha entre 'anos_iniciais' ou 'anos_finais'.")
  }

  # Verificar se o arquivo existe, se não, baixar
  if (download || !file.exists(file_path)) {
    # Diretório onde os dados brutos serão armazenados
    dir_path <- 'data_raw/'

    # Se o diretório não existir, cria-lo
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }

    # Fazer o download dos arquivos
    path <- .download_file(url, dir_path)

    # Extrair os arquivos ZIP
    .extract_zip(path, dir_path)

    # Remover os arquivos ZIP após a extração
    .remove_zip(path)
  }

  # Mensagem inicial para carregamento do arquivo
  message('Carregando dados do INEP a partir do arquivo: ', file_path)

  # Ler os dados, pulando as 8 primeiras linhas para evitar cabeçalhos desnecessários
  inep_data <- readxl::read_xlsx(
    file_path,
    col_names = TRUE,
    skip = 9
  )
  message('Arquivo carregado com sucesso.')

  # Filtrar e selecionar as colunas relevantes dos dados do IDEB
  message('Filtrando e selecionando as colunas relevantes...')
  ideb_data <- inep_data %>%
    dplyr::select(CO_MUNICIPIO,
                  NO_MUNICIPIO,
                  SG_UF,
                  REDE,
                  !!paste0("VL_OBSERVADO_", ano)) %>%
    dplyr::rename(`municipio_codigo` = CO_MUNICIPIO,
                  `municipio_nome` =  NO_MUNICIPIO,
                  `estado_sigla` = SG_UF,
                  !!paste0("ideb_", ano, "_", tipo) := !!paste0("VL_OBSERVADO_", ano)) %>%
    dplyr::filter(REDE == 'Pública')

  # Filtrar os dados do IDEB para manter apenas as linhas das cidades-alvo
  message('Filtrando dados para as cidades-alvo...')
  ideb_data <- ideb_data %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  # Retornar os dados do IDEB filtrados
  message('Dados filtrados e processados com sucesso.')
  return(ideb_data)
}


#' Obter Taxa de Abandono Escolar por Município
#'
#' Esta função permite obter os dados de taxa de abandono escolar fornecidos pelo INEP para o ano especificado.
#' Os dados podem ser baixados automaticamente, caso não estejam presentes no diretório local.
#'
#' @param download Um valor lógico (TRUE ou FALSE). Se TRUE, a função fará o download dos arquivos caso eles não existam
#'   localmente. Caso contrário, apenas tentará carregar os dados já existentes.
#' @param ano Um valor numérico ou caractere indicando o ano para o qual os dados devem ser obtidos. Os anos disponíveis
#'   são: 2007 até 2023.
#'
#' @return Um data frame contendo os dados de taxa de abandono escolar, com as colunas: `municipio_codigo`, `municipio_nome`,
#'   `estado_sigla`, `taxa_abandono_fundamental_<ano>`, `taxa_abandono_medio_<ano>`, onde `<ano>` é o ano solicitado.
#'
#' @examples
#' \dontrun{
#'   # Obter taxa de abandono escolar para o ano de 2023
#'   taxa_abandono_2023 <- et_taxa_abandono_escolar(download = TRUE, ano = "2023")
#'
#'   # Obter taxa de abandono escolar para o ano de 2022 sem baixar o arquivo
#'   taxa_abandono_2022 <- et_taxa_abandono_escolar(download = FALSE, ano = "2022")
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
    "2018" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2018/tx_rend_municipios_2018.zip",
    "2017" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2017/tx_rend_municipios_2017.zip",
    "2016" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2016/tx_rend_municipios_2016.zip",
    "2015" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2015/tx_rend_municipios_2015.zip",
    "2014" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2014/tx_rend_municipios_2014.zip",
    "2013" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2013/tx_rend_municipios_2013.zip",
    "2012" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2012/tx_rend_municipios_2012.zip",
    "2011" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2011/tx_rend_municipios_2011.zip",
    "2010" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2010/tx_rend_municipios_2010.zip",
    "2009" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2009/tx_rend_municipios_2009.zip",
    "2008" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2008/tx_rend_municipios_2008.zip",
    "2007" = "https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2007/tx_rend_municipios_2007.zip"
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

  # Filtrar apenas para os municípios-alvo (target_cities)
  message('Filtrando dados para os municípios-alvo...')
  taxa_abandono <- taxa_abandono %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  message('Dados filtrados e processados com sucesso.')
  return(taxa_abandono)
}
