#' Obter Dados de Empregos Formais Totais por Ano
#'
#' Esta função processa os dados da RAIS (Relação Anual de Informações Sociais) para retornar
#' o número total de empregos formais para cidades-alvo específicas em um determinado ano.
#' Ela faz o download (opcional) e lê os dados da RAIS, filtra-os com base nas cidades-alvo
#' predefinidas, e retorna as informações relevantes.
#'
#' @param download Lógico indicando se deve baixar os dados caso não existam localmente
#'        (padrão: TRUE)
#' @param ano Ano dos dados a serem obtidos (padrão: 2022). Atualmente suporta
#'        os anos de 2020 a 2024.
#'
#' @return Um data frame contendo:
#' \describe{
#'   \item{municipio_codigo}{Código IBGE do município}
#'   \item{municipio_nome}{Nome do município}
#'   \item{estado_sigla}{Sigla da UF do município}
#'   \item{total_empregos_formais_[ano]}{Número total de empregos formais no ano especificado}
#' }
#'
#' @importFrom dplyr select rename filter mutate across everything
#' @importFrom readxl read_xlsx
#' @importFrom glue glue
#' @export
#'
#' @examples
#' # Obter total de empregos formais para as cidades-alvo em 2022
#' total_empregos <- get_empregos_formais_total(ano = 2022)
#' head(total_empregos) # A coluna será chamada total_empregos_formais_2022
get_empregos_formais_total <- function(download = TRUE, ano = 2022) {

  # Validação do ano de entrada
  anos_disponiveis <- 2020:2024
  ano <- as.character(ano)

  if (!ano %in% as.character(anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ",
         paste(anos_disponiveis, collapse = ", "))
  }

  # Configuração dos caminhos de arquivo
  diretorio_dados <- 'data_raw/RAIS/'
  dir.create(diretorio_dados, showWarnings = FALSE, recursive = TRUE)
  caminho_arquivo <- glue('{diretorio_dados}tabela_rais_{ano}.xlsx')

  # Download se solicitado ou se arquivo não existir
  if (!file.exists(caminho_arquivo) || download) {
    tryCatch({
      caminho_arquivo <- download_RAIS(ano = ano)
    }, error = function(e) {
      stop("Falha ao baixar dados da RAIS: ", e$message)
    })
  }

  # Leitura dos dados
  dados_rais <- read_xlsx(
    caminho_arquivo,
    col_names = TRUE,
    sheet = "TABELA 4",
    range = "A13:AB5586"
  )

  # Carregar dados das cidades-alvo
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))

  # Converter municipio_nome para maiúsculo em target_cities
  target_cities <- target_cities %>%
    dplyr::mutate(municipio_nome = toupper(municipio_nome)%>%
                    stringi::stri_trans_general("Latin-ASCII") %>%  # Remove acentos
                    trimws()  # Remove espaços extras
                  )

  # Mapeamento de colunas por ano
  col_total_empregos <- list(
    '2024' = '...26',
    '2023' = '...26',
    '2022' = 'Total',
    '2021' = '...25',
    '2020' = 'Total'
  )

  # Nome da coluna dinâmica
  nome_coluna <- glue("total_empregos_formais_{ano}")

  # Processamento dos dados
  total_empregos <- dados_rais %>%
    dplyr::select(
      estado_sigla = `UF`,
      municipio_nome = `Município`,
      !!nome_coluna := !!col_total_empregos[[ano]]
    ) %>%
    dplyr::filter(!is.na(estado_sigla), !is.na(municipio_nome)) %>%
    dplyr::mutate(
      across(c(estado_sigla, municipio_nome), as.character),
      across(!!nome_coluna, as.numeric),
      municipio_nome = toupper(municipio_nome) %>%   # Converter para maiúsculo
      stringi::stri_trans_general("Latin-ASCII") %>%  # Remove acentos
        trimws()  # Remove espaços extras
    ) %>%
    dplyr::semi_join(target_cities, by = c("municipio_nome", "estado_sigla")) %>%
    dplyr::left_join(
      target_cities %>%
        dplyr::select(municipio_nome, estado_sigla, municipio_codigo),
      by = c("municipio_nome", "estado_sigla")
    ) %>%
    dplyr::select(municipio_codigo, municipio_nome, estado_sigla, everything())

  return(total_empregos)
}
