#' Salva os gastos com educação por município a partir dos arquivos CSV originais
#'
#' Esta função carrega e processa os dados de gastos públicos com educação
#' a partir de arquivos CSV baixandos manualmente na plataforma da sinconfi e
#' salvos dentro de <data_raw/finbra_despesas_por_funcao/>, renomeando os arquivos para <finbra_despesas_por_funcao_{ano}>,
#' filtrando os dados por município e consolidando os valores.
#' Em seguida, ela salva os arquivos no formato RData dentro de <data/>.
#'
#' Os dados foram retirados de <https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra/finbra_list.jsf>
#'
#' @param ano Integer. Ano dos dados a serem carregados (entre 2013 e 2024). Default: 2022.
#' @return Dataframe com os municípios e seus respectivos gastos com educação.
#' @examples
#' gastos_educacao <- save_gastos_totais_educacao(ano = 2023)
#' head(gastos_educacao)
#'
#' @import tidyverse
#' @importFrom readr read_delim locale
save_gastos_educacao_from_csv <- function(ano = 2022) {
  anos_disponiveis <- as.character(2013:2024)
  ano <- as.character(ano)

  if (!(ano %in% anos_disponiveis)) {
    stop("Ano inválido. Os anos disponíveis são: ", paste(anos_disponiveis, collapse = ", "))
  }

  file_path <- paste0('data_raw/finbra_despesas_por_funcao/finbra_despesas_por_funcao_', ano, '.csv')

  if  (!file.exists(file_path)){
    stop("O arquivo CSV não foi encontrado! Baixe ele manualmente no site da sinconfi e salve em
         <data_raw/finbra_despesas_por_funcao/> com o nome <finbra_despesas_por_funcao_{ano}>.", call. = FALSE)
  }

  message("Carregando a base de dados...")
  df <- readr::read_delim(file_path,
                          delim = ";",
                          locale = locale(encoding = "ISO-8859-1"),
                          skip = 3,
                          show_col_types = FALSE)
  message("Base de dados carregada com sucesso!")

  message("Filtrando dados...")
  gastos_totais_edu <- df %>%
    filter(Conta == '12 - Educação') %>%
           #Coluna %in% c('Despesas Empenhadas', 'Despesas Liquidadas', 'Despesas Pagas')) %>%
    pivot_wider(names_from = Coluna,
                values_from = Valor)


  #message("Filtrando cidades...")
  gastos_totais_edu <- gastos_totais_edu %>%
    mutate(`Cod.IBGE` = as.character(`Cod.IBGE`))

  #message("Calculando gastos totais com educação...")
  #gastos_totais_edu <- gastos_totais_edu %>%
  #  group_by(`Cod.IBGE`) %>%
  #  summarise(gastos_totais_educacao = sum(as.double(gsub(",", ".", Valor))), .groups = 'drop') %>%
  #  rename(municipio_codigo = `Cod.IBGE`)

  message("Selecionando colunas...")
  gastos_totais_edu <- gastos_totais_edu %>%
    select(-Conta, - `Identificador da Conta`)
    #right_join(target_cities, by = 'municipio_codigo') %>%
    #select(municipio_codigo, municipio_nome, estado_sigla, gastos_totais_educacao)



  message("Salvando arquivo RData...")
  path = "data/despesas_educacao/"
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  save(gastos_totais_edu, file = paste0(path, 'despesas_educacao_', ano,".RData"))

  message("Concluído!")
  return(gastos_totais_edu)
}
