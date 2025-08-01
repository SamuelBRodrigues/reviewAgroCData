#' Obter os gastos per capita em educação por município
#'
#' Esta função coleta os dados de gastos per capita em educação para os municípios
#' especificados, consultando a API da FGV.
#'
#' @param cod_ibge Vetor de códigos IBGE dos municípios a serem consultados.
#'                 O padrão é `target_cities$municipio_codigo`. Deve ser um vetor numérico.
#'
#' @return Um `data.frame` contendo os dados de gastos per capita em educação
#'         para os municípios consultados, incluindo as colunas:
#'         - `municipio_codigo`: Código IBGE do município.
#'         - `ano`: Ano da observação.
#'         - `gastos_per_capita_educacao`: Valor do gasto per capita em educação.
#'         - Demais colunas retornadas pela API, com nomes padronizados.
#'
#' @examples
#'
#' # Obter os gastos per capita em educação para os municípios padrão
#' get_gastos_per_capita_educacao()
#'
#' # Obter os gastos per capita em educação para municípios específicos
#' get_gastos_per_capita_educacao(c(3550308, 3304557))
#'
#' @export
get_gastos_per_capita_educacao <- function(cod_ibge = target_cities$municipio_codigo) {
  message("Coletando dados...")

  reqs <- purrr::map(
    cod_municipios_ibge,
    ~{
      # Url dos dados
      url <- "https://analitica.municipios.fgv.br/"

      # Parametros a serem enviados para API
      query_params <- list(
        callback = "jQuery364009964332437813739_1743094193524",
        api_ticket = "b09981dc947ed0b1e3a8371eeaf67178c5caf20ee90bcea702cb8c2a19145d3a",
        channel = "default",
        api_indicator = "8efb100a295c0c690931222ff4467bb8",
        filters = stringr::str_glue("co_municipio,2,'{.x}'")
      )

      # Construindo a requisição
      req <- httr2::request(url) |>
        httr2::req_url_query(!!!query_params) |>
        httr2::req_headers(
          "accept" = "*/*",
          "accept-language" = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
          "cookie" = "_ga=GA1.1.1812424873.1742563784; _ga_LDYK37ZKX2=GS1.1.1743094194.4.0.1743094194.0.0.0",
          "referer" = "https://municipios.fgv.br/",
          "sec-ch-ua" = "\"Not A(Brand\";v=\"8\", \"Chromium\";v=\"132\", \"Opera GX\";v=\"117\"",
          "sec-ch-ua-mobile" = "?0",
          "sec-ch-ua-platform" = "\"Windows\"",
          "sec-fetch-dest" = "script",
          "sec-fetch-mode" = "no-cors",
          "sec-fetch-site" = "same-site",
          "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36 OPR/117.0.0.0"
        ) |>
        httr2::req_retry(5) |> # Tentativas de requisição
        httr2::req_throttle(60, realm = url) # Limite de requisiçõesp por minuto
      # Retornando a requisição
      return(req)
    }
  )
  # Performando as requisições em paralelo
  resps <- httr2::req_perform_parallel(reqs,
                                       max_active = 10, # Limite de requisições em paralelo
                                       progress = TRUE
  )

  # Extraindo os dados
  extract_function <- function(resp){

    # Lendo a resposta da API como um texto
    body <- resp |> httr2::resp_body_string()
    cod_munic <- resp$request$url |> stringr::str_extract("(?<=27)[:digit:]{7}(?=%)")
    # Estruturar a resposta como um json
    jsonp_response <- stringr::str_match(body, "\\((.*)\\)")[, 2]

    # Converter JSON para lista
    json <- jsonlite::fromJSON(jsonp_response)

    # Estruturando os dados em uma tabela
    data <- json$dados |>
      tibble::tibble() |>
      dplyr::mutate(
        municipio_codigo = cod_munic
      )
  }
  # Aplicando a função de extração a cada resposta
  dataset <- httr2::resps_data(
    resps,
    resp_data = \(resp) extract_function(resp)
  )

  data_treated <- dataset |>
    dplyr::select(municipio_codigo, NO_MUNICIPIO, ANO, VALOR_PERCAPTA) |>
    dplyr::mutate(
      ANO = as.integer(ANO)
    ) |>
    dplyr::rename(
      ano = ANO,
      municipio_nome = NO_MUNICIPIO,
      gasto_per_capta_saude = VALOR_PERCAPTA
    )

  # Checando se algum municipio não retornou dados

  null_mun <- setdiff(cod_municipios_ibge, data_treated$municipio_codigo)

  if(length(null_mun) > 0){
    warning(
      "Alguns municípios não retornaram dados: ",
      paste(null_mun, collapse = ", ")
    )
  }

  return(data_treated)
}
