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
    dplyr::filter(REDE == 'Pública') %>%
    dplyr::mutate(municipio_codigo = as.character((municipio_codigo)))

  # Retornar os dados do IDEB filtrados
  message('Dados filtrados e processados com sucesso.')
  return(ideb_data)
}
