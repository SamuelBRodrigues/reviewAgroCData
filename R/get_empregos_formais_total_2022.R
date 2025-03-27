#' Obter Dados de Empregos Formais Totais para 2022
#'
#' Esta função processa os dados da RAIS (Relação Anual de Informações Sociais) para retornar
#' o número total de empregos formais para cidades-alvo específicas em 2022.
#' Ela faz o download e lê os dados da RAIS, filtra-os com base nas cidades-alvo predefinidas,
#' e retorna as informações relevantes.
#'
#' @return Um data frame contendo o código do município (`municipio_codigo`), nome do município (`municipio_nome`),
#'         sigla do estado (`estado_sigla`) e o total de empregos formais (`total_empregos`) para as cidades-alvo.
#' @import dplyr
#' @import readxl
#' @export
#'
#' @examples
#' # Obter total de empregos formais para as cidades-alvo em 2022
#' total_empregos_2022 <- get_empregos_formais_total_2022()
#' head(total_empregos_2022)
get_empregos_formais_total_2022 <- function(){

  file_path = download_RAIS(dir_path = "data_raw/")
  message('Loading RAIS data from file: ', file_path)

  # Ler os dados
  rais_data <- read_xlsx(
    file_path,
    col_names = TRUE,
    sheet = "TABELA 4",
    range = "A13:O5586"
  )

  message('File loaded successfully.')

  # Carregar os dados das cidades-alvo do pacote
  message('Loading target cities data...')
  load(system.file("data", "target_cities.rda", package = "reviewAgroCData"))
  message('Target cities data loaded.')

  # Filtrar e selecionar colunas relevantes dos dados da RAIS
  message('Filtering and selecting relevant columns...')
  total_empregos_2022 <- rais_data %>%
    dplyr::select(`UF`,
                  `Município`,
                  `Total`) %>%
    slice(-(1:2)) %>%
    dplyr::rename(`municipio_nome` =  Município,
                  `estado_sigla` = UF,
                  `total_empregos` = `Total`)

  # Filtrar os dados da RAIS para manter apenas as linhas das cidades-alvo
  message('Filtering data for target cities...')
  total_empregos_2022 <- total_empregos_2022 %>%
    semi_join(target_cities, by = c("municipio_nome", "estado_sigla")) %>%
    left_join(target_cities %>% select(municipio_nome, estado_sigla, municipio_codigo),
              by = c("municipio_nome", "estado_sigla")) %>%
    select(municipio_codigo, municipio_nome, estado_sigla, total_empregos)

  # Retornar os dados filtrados
  message('Data filtered and processed successfully.')
  return(total_empregos_2022)
}
