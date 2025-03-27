#' Extração dos dados de cadastros do CadÚnico e Bolsa Família
#'
#' Essa função extrai os dados de unidades de famílias cadastradas no CadÚnico e
#' Bolsa Família a nível municipal segundo a plataforma do Aplicações Cidadania.
#'
#' @param cod_municipios_ibge Lista com o código dos munícipios do IBGE. Como padrão
#' utiliza a coluna municipios codigo da tabela target_cities.
#' @param ano Ano de interesse. Por padrão usa o ano de 2023
#' @param mes Mês de interesse. Por padrão usa o mes de junho (06)
#'
#' @returns Um data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   get_cad_bf_data(
#'     cod_municipios_ibge = c("1508126", "1708205", "2101400", "2112001"),
#'     ano = "2020",
#'     mes = "07"
#'    )
#' }
get_cad_bf_data <- function(cod_municipios_ibge = target_cities$municipio_codigo, ano = "2023", mes = "06"){

  data <- purrr::map_df(cod_municipios_ibge,
                        ~{

                          cod_ibge <- .x |> stringr::str_extract("^.*(?=.$)")
                          # Mimetizar comportamento humano
                          Sys.sleep(runif(1, 2,5))

                          # Url dos dados do cadunico
                          url_cadunico <- stringr::str_glue("https://aplicacoes.mds.gov.br/sagi/RIv3/geral/conteudo_modulo.php?id=2109&ibge={cod_ibge}&area=&ano={ano}&mes={mes}&ct_captcha=RIPBFPATS&ctidr=")

                          # Url dos dados do Bolsa Família
                          url_bf <- stringr::str_glue("https://aplicacoes.mds.gov.br/sagi/RIv3/geral/conteudo_modulo.php?id=2111&ibge={cod_ibge}&area=&ano={ano}&mes={mes}&ct_captcha=RIPBFPATS&ctidr=")

                          # Pegando os dados do CadUnico ------------
                          # Lendo o html dos dados do cadunico
                          html_cadunico <- rvest::read_html(url_cadunico)

                          # Pegando os valores de interesse
                          values_cadunico <- html_cadunico |>
                            rvest::html_element(
                              css = "body > ul:nth-child(5)"
                            ) |>
                            rvest::html_elements("strong") |>
                            rvest::html_text2()

                          # Pegando as variaveis de interesse
                          variables <- html_cadunico |>
                            rvest::html_element(
                              css = "body > ul:nth-child(5)"
                            ) |>
                            rvest::html_elements("span") |>
                            rvest::html_text2() |>
                            stringr::str_extract("[^0-9\\.\\;\\-]+") |>
                            stringr::str_trim(side = "left")

                          # Pegando os dados do Bolsa Família -------------
                          # Lendo o html do dados do Bolsa Família
                          html_bf <- rvest::read_html(url_bf)

                          # Pegando os valores de famílias que receberam BF no município
                          values_bf <- html_bf |>
                            rvest::html_node(
                              css = "body > div:nth-child(1) > p:nth-child(4)"
                            ) |>
                            rvest::html_text2() |>
                            stringr::str_remove("[.]") |>
                            stringr::str_extract("[:digit:]+(?=\\s)")

                          # Construindo uma tabela com os dados
                          data <- tibble::tibble(
                            cod_ibge = .x,
                            variables = variables,
                            values = values_cadunico,
                            ano = "2023",
                            mes = "06"
                          ) |>
                            dplyr::mutate(
                              values = stringr::str_replace(values, "\\.",""),
                              values = as.numeric(values)
                            ) |>
                            dplyr::left_join(
                              pop_municipios |>
                                tidyr::unite(
                                  "cod_ibge",
                                  cod_uf:cod_munic,
                                  sep = ""
                                ) |>
                                dplyr::select(
                                  cod_ibge, nome_do_municipio
                                )
                            ) |>
                            dplyr::relocate(
                              cod_ibge, nome_do_municipio, ano, mes
                            ) |>
                            tidyr::pivot_wider(
                              names_from = variables,
                              values_from = values
                            ) |>
                            janitor::clean_names() |>
                            dplyr::mutate(
                              familias_beneficiaria_bolsa_familia = values_bf |> as.numeric(),
                              proporcao_bf_ate_meio_sm = familias_beneficiaria_bolsa_familia/familias_com_renda_ate_1_2_salario_minimo,
                              proporcao_ate_meio_sm_reg_cadunico = familias_com_renda_ate_1_2_salario_minimo/familias_inseridas_no_cadastro_unico
                            )

                          message(stringr::str_glue("Extraindo os dados: {data$nome_do_municipio}"))
                          return(data)

                        }
  )
}

#' Extração o número de familias e pessoas em situação de pobreza do Cadúnico
#'
#' Essa função extrai o número de pessoas e famílias em situação de pobreza e que
#' estão cadastradas no CadÚnico da plataforma Aplicações Cidadania.
#'
#' @param cod_municipios_ibge Lista com o código dos munícipios do IBGE. Como padrão
#' utiliza a coluna municipios codigo da tabela target_cities.
#'
#' @returns Um data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'  get_situacao_pobreza_data(cod_municipios_ibge = c("1508126", "1708205", "2101400"))
#' }
get_situacao_pobreza_data <- function(cod_municipios_ibge = target_cities$municipio_codigo){

  purrr::map_df(
    cod_municipios_ibge,
    ~{
      # Pegando os 6 primeiros digites do código do municipio
      cod_ibge <- stringr::str_extract(.x, "^.*(?=.$)")

      # Construindo o url que contém os dados do município
      url <- stringr::str_glue("https://cecad.cidadania.gov.br/painel03.php?&oik=1&p_ibge={cod_ibge}")

      # Lendo o html do site
      html <- rvest::read_html(url)

      # Pegano o mês do dado consultado
      mes <- html |>
        rvest::html_element(
          css = "#dados-cadastro-dois"
        ) |>
        rvest::html_element(
          css = "p"
        ) |>
        rvest::html_text2() |>
        stringr::str_extract("\\d+")
      # Pegando o ano do dado consultado
      ano <- html |>
        rvest::html_element(
          css = "#dados-cadastro-dois"
        ) |>
        rvest::html_element(
          css = "p"
        ) |>
        rvest::html_text2() |>
        stringr::str_extract("\\d+$")

      # Pegando o valor de familia e pessoas cadastradas no CadÚnico em situação de pobreza
      situacao_pobreza <- html |>
        rvest::html_elements(
          css = "#dados-cadastro-dois > div:nth-child(2) > div > div:nth-child(1) > div"
        ) |>
        rvest::html_elements("h5") |>
        rvest::html_text() |>
        # Removendo a pontuação
        stringr::str_replace('\\.', "") |>
        stringr::str_extract('\\d+')

      # Famílias em situação de pobreza
      familias_situacao_pobreza <- situacao_pobreza[1]

      # Pessoas em situação de pobreza
      pessoas_situacao_pobreza <- situacao_pobreza[2]

      # Construindo uma tabela com os dados
      data <- tibble::tibble(
        cod_ibge = .x,
        ano = ano,
        mes = mes,
        familias_situacao_pobreza = familias_situacao_pobreza |> as.numeric(),
        pessoas_situacao_pobreza = pessoas_situacao_pobreza |> as.numeric()
      ) |>
        dplyr::left_join(
          pop_municipios |>
            tidyr::unite(
              "cod_ibge",
              cod_uf:cod_munic,
              sep = ""
            ) |>
            dplyr::select(
              cod_ibge, nome_do_municipio
            )
        ) |>
        dplyr::relocate(
          cod_ibge, nome_do_municipio, ano, mes
        )

    }
  )

}

#' Download dos dados do SUAS - CRAS
#'
#' A função baixa os dados do CRAS da bases do senso SUAS
#'
#' @param dir diretório onde os dados serão baixados
#' @param ano Ano de interesse dos dados
#'
#' @returns Um arquivo compactado no diretorio indicado
#' @export
#'
#' @examples
#' \dontrun{
#'   download_cras_data(dir = "data_raw", ano = "2022")
#' }
download_cras_data <- function(dir = "data_raw", ano = "2022"){

  # Lista nomeada com os links de download dos dados
  links <- list(
    "2023" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/1%20-%20CRAS(4).rar",
    "2022" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/1_CRAS.rar",
    "2021" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/1%20-%20CRAS(1).zip",
    "2020" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/1_CRAS_2020.zip",
    "2019" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(5).zip",
    "2018" = "https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(3).zip",
    "2017" = "http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS/2017/Censo_SUAS_2017_CRAS.zip",
    "2016" = "http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS_2016/CensoSUAS2016_CRAS.zip",
    "2015" = "http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS_2015/CensoSUAS2015_CRAS.zip"
  )

  # Url do download de acordo com o ano
  url <- links[ano] |> purrr::pluck(1)

  # Tipo do arquivo baixado
  file_type <- url |> stringr::str_extract("\\.(rar|zip)$")

  # Nome do arquivo baixado
  file_name <- stringr::str_glue("censo_suas_cras_{ano}{file_type}")

  # Caminho do arquivo
  path <- stringr::str_glue("{dir}/cras/{ano}")

  if(!dir.exists(path)){
    dir.create(path, recursive = TRUE)
  }

  # Baixando os dados
  options(timeout = 600) #aumentando o tempo limite de donwload
  download.file(url,
                mode = "wb",
                destfile = stringr::str_glue("{path}/{file_name}")
  )

}

#' Trata os dados do CRAS
#'
#' A função trata os dados do CRAS já baixados e extraídos no mesmo diretório
#' indicado de download
#'
#' @param dir Diretório onde os dados foram baixados
#' @param ano Ano de interesse dos dados a serem tratados
#'
#' @returns Uma tabela com os dados do CRAS a nível municipal
#' @export
#'
#' @examples
#' \dontrun{
#'   extract_cras_data(dir = "data_raw", ano = "2022")
#' }
extract_cras_data <- function(dir = "data_raw", ano = "2022"){

  file_folder_list <- list(
    "2023" = "1%20-%20CRAS(4)",
    "2022" = "1_CRAS",
    "2021" = "1%20-%20CRAS(1)",
    "2020" = "1_CRAS_2020",
    "2019" = "CRAS(5)",
    "2018" = "CRAS(3)",
    "2017" = "Censo_SUAS_2017_CRAS",
    "2016" = "CensoSUAS2016_CRAS",
    "2015" = "CensoSUAS2015_CRAS"
  )

  file_folder <- file_folder_list[ano] |> purrr::pluck(1)

  path <- stringr::str_glue("{dir}/cras/{ano}/{file_folder}")

  file_path <- list.files(path,
                          pattern = "(Divulgação|divulgacao).xlsx",
                          full.names = T)

  data <- readxl::read_xlsx(file_path) |>
    dplyr::rename(
      "cras" = q01,
      "municipio_nome" = q09,
      "estado_sigla" = q010
    ) |>
    dplyr::group_by(
      estado_sigla, municipio_nome
    ) |>
    dplyr::summarise(
      cras = n()
    ) |>
    dplyr::left_join(
      pop_municipios |>
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = ""
        )
      ,
      by = join_by(municipio_nome == nome_do_municipio, estado_sigla == uf)
    ) |>
    dplyr::mutate(
      cras_por_100k_hab = (cras/populacao) * 100000
    ) |>
    dplyr::relocate(
      municipio_codigo, municipio_nome, cras, populacao, cras_por_100k_hab
    )

  return(data)
}
