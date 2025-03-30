#' Extração dos dados de Cobertura de Atenção Primária a Saúde (APS)
#'
#' A função extrai os dados de Cobertura APS diretamente da plataforma e-Gestor
#' do ministério da Saúde. Os dados são detalhados a nível municipal para o ano
#' e mês inserido
#'
#' @param ano Ano de interesse dos dados. Por padrão usa o ano de 2024
#' @param mes Mês de interesse dos dados. Por padrão usa o mês de "04", abril
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   dados_cobertura_aps <- get_cobertura_aps(
#'     ano = "2024",
#'     mes = "01"
#'   )
#' }
get_cobertura_aps <- function(ano = "2024", mes = "04"){

  # URL que contém os dados
  url <- "https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/relCoberturaAPSCadastro.xhtml"

  #Construindo o body_raw da requisição
  body_raw <- stringr::str_glue("j_idt58=j_idt58&javax.faces.ViewState=4753233255254307441%3A5083794241351514494&j_idt58%3AtipoConsulta=uniGeoComp&j_idt58%3AunidGeo=municipio&j_idt58%3Aregiao=00&j_idt58%3Aestados=00&j_idt58%3Amunicipios=00&j_idt58%3AcompInicio={ano}{mes}&j_idt58%3AverTela=Ver+em+tela&example_length=10")


  # Construindo a requisição para API
  req <- httr2::request(url) |>
    httr2::req_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
      `Accept-Language` = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
      `Cache-Control` = "max-age=0",
      Connection = "keep-alive",
      `Content-Type` = "application/x-www-form-urlencoded",
      Cookie = "BIGipServeregestorab_prod=1946230188.8225.0000; _ga_PQPQJHMHNQ=GS1.1.1743214483.2.0.1743214483.0.0.0; _ga_TVTCQ70GPK=GS1.1.1743247277.1.1.1743247289.0.0.0; _ga_XSF0K1N5G9=GS1.1.1743247150.1.1.1743248093.0.0.0; JSESSIONID=ogTZyWkB13uwVnHdRudI6aDJ; _gid=GA1.3.456297898.1743302954; _gat_gtag_UA_117915284_1=1; _ga=GA1.1.1753857876.1743187813; _ga_YG7RPWS2GW=GS1.1.1743302954.5.1.1743303422.0.0.0",
      Origin = "https://egestorab.saude.gov.br",
      Referer = "https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/relCoberturaAPSCadastro.xhtml",
      `Sec-Fetch-Dest` = "document",
      `Sec-Fetch-Mode` = "navigate",
      `Sec-Fetch-Site` = "same-origin",
      `Sec-Fetch-User` = "?1",
      `Upgrade-Insecure-Requests` = "1",
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36 OPR/117.0.0.0",
      `sec-ch-ua` = '"Not A(Brand";v="8", "Chromium";v="132", "Opera GX";v="117"',
      `sec-ch-ua-mobile` = "?0",
      `sec-ch-ua-platform` = '"Windows"'
    ) |>
    httr2::req_method("POST") |>  # Definindo como POST
    httr2::req_body_raw(
      body_raw,
      "application/x-www-form-urlencoded"
    )

  # Performando a requisição
  resp <- httr2::req_perform(req)

  # Transformando a resposta em uma tabela
  tabela <- resp |>
    httr2::resp_body_html() |>
    rvest::html_table() |>
    purrr::pluck(3) # Dentre as tabelas, a tabela de interesse é a terceira

  # Tratando a tabela para os dados de interesse
  data <- tabela |>
    janitor::clean_names() |>
    dplyr::select(
      competencia_cnes, ibge, municipio, cobertura_aps
    ) |>
    dplyr::rename(
      "time_period" = competencia_cnes,
      "municipio_codigo" = ibge
    )

  return(data)
}
