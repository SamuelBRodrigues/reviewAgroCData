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

  # Filtrar apenas para os municípios-alvo (target_cities)
  message('Filtrando dados para os municípios-alvo...')
  taxa_abandono <- taxa_abandono %>%
    dplyr::filter(municipio_codigo %in% target_cities$municipio_codigo)

  message('Dados filtrados e processados com sucesso.')
  return(taxa_abandono)
}

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

  # Definir o caminho do arquivo
  file_path = 'data_raw/Taxa líquida de matrículas em creches.xlsx'
  message('Carregando dados do arquivo: ', file_path)

  # Ler os dados da planilha
  taxa_matricula <- read_xlsx(
    file_path,
    col_names = TRUE,
    sheet = '1 -   Proporção de crianças ent',
    skip = 2,
    na = "-"
  )
  message('Arquivo carregado com sucesso.')

  # Carregar os dados das cidades-alvo do pacote
  message('Carregando dados das cidades-alvo...')
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))
  message('Dados das cidades-alvo carregados.')

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


#' Obter gastos totais com educação por município
#'
#' Esta função carrega e processa os dados de gastos públicos com educação
#' a partir de arquivos CSV, filtrando por município e consolidando os valores.
#'
#' Os dados foram retirados de https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra/finbra_list.jsf
#'
#' @param ano Integer. Ano dos dados a serem carregados (entre 2013 e 2024). Default: 2022.
#' @return Dataframe com os municípios e seus respectivos gastos totais com educação.
#' @examples
#' gastos_educacao <- get_gastos_totais_educacao(ano = 2023)
#' head(gastos_educacao)
#'
#' @importFrom readr read_delim locale
#' @export
get_gastos_totais_educacao <- function(ano = 2022) {
  anos_disponiveis <- as.character(2013:2024)
  ano <- as.character(ano)

  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ", paste(anos_disponiveis, collapse = ", "))
  }

  file_path <- paste0('data_raw/finbra_despesas_por_funcao/finbra_despesas_por_funcao_', ano, '.csv')

  message("Carregando a base de dados...")
  df <- readr::read_delim(file_path,
                          delim = ";",
                          locale = locale(encoding = "ISO-8859-1"),
                          skip = 3,
                          show_col_types = FALSE)
  message("Base de dados carregada com sucesso!")

  message("Filtrando dados...")
  gastos_totais_edu <- df %>%
    filter(Conta == '12 - Educação',
           Coluna %in% c('Despesas Empenhadas', 'Despesas Liquidadas', 'Despesas Pagas'))

  message("Filtrando cidades...")
  gastos_totais_edu <- gastos_totais_edu %>%
    mutate(`Cod.IBGE` = as.character(`Cod.IBGE`)) %>%
    filter(`Cod.IBGE` %in% target_cities$municipio_codigo)

  message("Calculando gastos totais com educação...")
  gastos_totais_edu <- gastos_totais_edu %>%
    group_by(`Cod.IBGE`) %>%
    summarise(gastos_totais_educacao = sum(as.double(gsub(",", ".", Valor))), .groups = 'drop') %>%
    rename(municipio_codigo = `Cod.IBGE`)

  message("Selecionando colunas...")
  gastos_totais_edu <- gastos_totais_edu %>%
    right_join(target_cities, by = 'municipio_codigo') %>%
    select(municipio_codigo, municipio_nome, estado_sigla, gastos_totais_educacao)

  message("Concluído!")
  return(gastos_totais_edu)
}


