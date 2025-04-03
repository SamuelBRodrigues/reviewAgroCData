#' Extração dos dados de Aquisição Agricultura Familiar pelo PNAE
#'
#' A função extrai e trata os dados de Aquisição Agricultura Familiar pelo PNAE de um
#' determinado ano a nível municipal. Os dados são extraídos diretamente do Fundo
#' Nacional de Desenvolvimento da Educação do gov.br.
#'
#' @param dir Diretório onde os arquivos serão salvos ou lidos.
#' @param ano Ano de interesse dos dados
#'
#' @returns Um dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#'   data_prop_alimentos_pnae <- get_alimentos_pnae(
#'     dir = "data/pnae",
#'     ano = 2022
#'   )
#' }
get_alimentos_pnae <- function(dir = "data_raw/pnae", ano = 2022){

  # Checando se o diretório existe
  if(!dir.exists(dir)){
    dir.create(dir, recursive = TRUE)
  }

  # Links de downloads
  link_download <- list(
    "2011" = "https://www.fnde.gov.br/phocadownload/programas/alimentacao_escolar/consultas/dados-da-agricultura-familiar/aquisicoes-agricultura-familiar-2011.xlsx",
    "2012" = "https://www.fnde.gov.br/phocadownload/programas/alimentacao_escolar/consultas/dados-da-agricultura-familiar/aquisicoes-agricultura-familiar-2012.xlsx",
    "2013" = "https://www.fnde.gov.br/phocadownload/programas/alimentacao_escolar/dados-agricultura-familiar/Planilha%202013_30_3_17.xlsx",
    "2014" = "https://www.fnde.gov.br/phocadownload/programas/alimentacao_escolar/dados-agricultura-familiar/Planilha%202014_30_3_17.xlsx",
    "2015" = "https://www.gov.br/fnde/pt-br/acesso-a-informacao/acoes-e-programas/programas/pnae/consultas/dados-agricultura-familiar-planilhas/DadosAF_2015.xlsx",
    "2016" = "https://www.fnde.gov.br/phocadownload/Planilha_AF2016_12_4_18.xlsx",
    "2017" = "https://www.fnde.gov.br/phocadownload/programas/alimentacao_escolar/2019/planilha_AF2017_10062019.xls",
    "2018" = "https://www.gov.br/fnde/pt-br/acesso-a-informacao/acoes-e-programas/programas/pnae/consultas/dados-agricultura-familiar-planilhas/DadosAF2018.xlsx",
    "2019" = "https://www.gov.br/fnde/pt-br/acesso-a-informacao/acoes-e-programas/programas/pnae/consultas/dados-agricultura-familiar-planilhas/ExtracaoAF_201910322_paraenvio.xlsx",
    "2020" = "https://www.gov.br/fnde/pt-br/acesso-a-informacao/acoes-e-programas/programas/pnae/consultas/dados-agricultura-familiar-planilhas/Planilha2020_04_3_24.xlsx",
    "2021" = "https://www.gov.br/fnde/pt-br/acesso-a-informacao/acoes-e-programas/programas/pnae/consultas/dados-agricultura-familiar-planilhas/Planilha2021_04_3_24.xlsx",
    "2022" = "https://www.gov.br/fnde/pt-br/acesso-a-informacao/acoes-e-programas/programas/pnae/consultas/dados-agricultura-familiar-planilhas/Planilha2022_04_3_24.xlsx"
  )

  # Link de download para o ano dado
  link <- link_download[as.character(ano)] |> purrr::pluck(1)

  # Destino do arquivo baixado
  file_dest <- stringr::str_glue("{dir}/aquisicoes_agricultura_familiar_pnae_{ano}.xlsx")

  if(!file.exists(file_dest)){
    utils::download.file(url = link,
                  mode = "wb",
                  destfile = file_dest)
    data <- readxl::read_xlsx(file_dest, skip = 1) |>
      janitor::clean_names()
  } else{
    message(stringr::str_glue("Os dados de Aquisição Agricultura Familiar de {ano} da PNAE já existe no diretório:
                         {file_dest}.
                            Lendo o arquivo"))
    data <- readxl::read_xlsx(file_dest, skip = 1) |>
      janitor::clean_names()
  }

  data <- data |>
    dplyr::mutate(
      percentual = ifelse(percentual == "100% ou mais", 1.00, percentual),
      percentual = as.numeric(percentual),
      entidade_executora = stringr::str_remove(entidade_executora, "PREF MUN DE "),
      ibge = as.character(ibge)
    ) |>
  dplyr::rename(
    "municipio_nome" = entidade_executora,
    "municipio_codigo" = ibge,
    "prop_alimentos_pnae" = percentual
  ) |>
    dplyr::select(
      municipio_nome, municipio_codigo, ano, prop_alimentos_pnae
    )



  return(data)

}
