#' Pega os dados de Telecomunicações dos Municipios Brasileiros
#'
#' Essa função pega os dados de Telecomunicações dos Municipios Brasileiros direto
#' dos dados abertos da ANATEL, salva os dados baixados no diretório destinado na
#' pasta "internet_data_anatel" e retorna um dataframe com os dados por município
#' no ano indicado.
#'
#' @param dir Diretório onde os dados serão baixados. Por padrão utiliza "data_raw"
#' @param ano Ano de interesse dos dados. Por padrão utliza 2024.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_internet_data(dir = "data", ano = 2022)
#' }
get_internet_data <- function(dir = "data_raw", ano = 2024){

  y <- ano
  dest <- stringr::str_glue("{dir}/internet_data_anatel")
  if(!dir.exists(dest)){
    dir.create(dest, recursive = T)
  }

  archive_dir <- stringr::str_glue("{dest}/telecomunicacoes_nos_municipios_brasileiros_anatel.zip")
  download.file("https://www.anatel.gov.br/dadosabertos/paineis_de_dados/meu_municipio/meu_municipio.zip",
                mode = "wb",
                destfile = archive_dir)

  zip::unzip(archive_dir,
             exdir = dest)


  cobertura_data <- stringr::str_glue("{dest}/Meu_Municipio_Cobertura.csv")
  cobertura <- readr::read_csv2(cobertura_data) |>
    janitor::clean_names() |>
    dplyr::filter(
      operadora == "Todas"
    ) |>
    dplyr::filter(
      tecnologia %in% c("4G", "5G")
    ) |>
    dplyr::select(
      municipio_codigo = codigo_ibge, ano, tecnologia, percent_area_coberta
    ) |>
    dplyr::mutate(
      municipio_codigo = as.character(municipio_codigo),
      ano = as.character(ano)
    ) |>
    tidyr::pivot_wider(
      names_from = tecnologia,
      values_from = percent_area_coberta,
      names_glue = "percet_area_coberta_{tecnologia}"
    ) |>
    dplyr::rename(
      !!stringr::str_glue("percet_area_coberta_5G_{ano}") := percet_area_coberta_5G,
      !!stringr::str_glue("percet_area_coberta_4G_{ano}") := percet_area_coberta_4G
    )

  acesso_data <- stringr::str_glue("{dest}/Meu_Municipio_Acessos.csv")
  acesso <- readr::read_csv2(acesso_data) |>
    janitor::clean_names() |>
    dplyr::select(
      municipio_codigo = codigo_ibge, ano, servico, acessos
    ) |>
    dplyr::filter(ano >= 2021) |>
    dplyr::mutate(
      municipio_codigo = as.character(municipio_codigo),
      ano = as.character(ano)
    ) |>
    dplyr::filter(
      servico %in% c("Telefonia Móvel", "Banda Larga Fixa")
    ) |>
    tidyr::pivot_wider(
      names_from = servico,
      values_from = acessos
    ) |>
    janitor::clean_names() |>
    dplyr::rename(
      !!stringr::str_glue("telefonia_movel_{ano}") := telefonia_movel,
      !!stringr::str_glue("banda_larga_fixa_{ano}") := banda_larga_fixa
    )

  data <- cobertura |>
    dplyr::left_join(
      acesso
    ) |>
    dplyr::filter(
      ano == ano
    ) |>
    select(-ano)

  return(data)
}
