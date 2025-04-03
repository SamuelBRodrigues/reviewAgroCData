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
get_cad_bf_data <- function(cod_municipios_ibge = target_cities$municipio_codigo, ano = "2025", mes = "03"){

  data <- purrr::map_df(cod_municipios_ibge,
                        ~{

                          cod_ibge <- .x |> stringr::str_extract("^.*(?=.$)")

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
                            ano = ano,
                            mes = mes
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
                          # Mimetizar comportamento humano
                          Sys.sleep(runif(1, 1,3))

                          dplyr::glimpse(data)
                          return(data)

                        }
  )
}
