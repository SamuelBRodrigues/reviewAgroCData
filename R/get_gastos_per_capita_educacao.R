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

  gastos_educacao <- purrr::map_df(
    target_cities$municipio_codigo,

    ~{

      # Mimetiza comportamento
      Sys.sleep(runif(1,1,3))

      # Url da API
      url <- "https://analitica.municipios.fgv.br/"

      # Codigo da cidade segundo ibge
      city_code_ibge <- .x
      # Definir os parâmetros da query
      query_params <- list(
        callback = "jQuery36403137933189103961_1743266780826",
        api_ticket = "b09981dc947ed0b1e3a8371eeaf67178c5caf20ee90bcea702cb8c2a19145d3a",
        channel = "default",
        api_indicator = "8efb100a295c0c690931222ff4467bb8",
        filters = stringr::str_glue("co_municipio,2,'{city_code_ibge}'")
      )

      # Construir a requisição
      resp <- httr2::request(url) |>
        httr2::req_url_query(!!!query_params) |>  # Adiciona os parâmetros de query
        httr2::req_headers(
          "accept" = "*/*",
          "accept-language" = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
          "cookie" = "_ga=GA1.1.1812424873.1742563784; _ga_LDYK37ZKX2=GS1.1.1743266521.7.1.1743266780.0.0.0",
          "referer" = "https://municipios.fgv.br/",
          "sec-ch-ua" = "\"Not A(Brand\";v=\"8\", \"Chromium\";v=\"132\", \"Opera GX\";v=\"117\"",
          "sec-ch-ua-mobile" = "?0",
          "sec-ch-ua-platform" = "\"Windows\"",
          "sec-fetch-dest" = "script",
          "sec-fetch-mode" = "no-cors",
          "sec-fetch-site" = "same-site",
          "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36 OPR/117.0.0.0"
        ) |>
        httr2::req_perform()
      # Lendo a resposta da API como um texto
      body <- resp |> httr2::resp_body_string()

      # Estruturar a resposta como um json
      jsonp_response <- body

      # Remover o callback e extrair apenas o JSON
      json_text <- sub("^[^(]*\\((.*)\\);?$", "\\1", jsonp_response)

      # Converter JSON para lista
      json_data <- jsonlite::fromJSON(json_text)


      data <- json_data$dados |>
        tibble::tibble() |>
        janitor::clean_names() |>
        dplyr::select(-ds_config) |>
        dplyr::mutate(
          municipio_codigo = .x
        ) |>
        dplyr::relocate(
          municipio_codigo,
          ano
        )

      message(stringr::str_glue("Os dados do municipio {data$no_municipio[1]} foram coletados"))

      return(data)
    }
  )

  # Renomeia a coluna final
  gastos_educacao <- gastos_educacao |>
    dplyr::rename(gastos_per_capita_educacao = valor_por_populacao,
                  municipio_nome = no_municipio)

  message("Dados coletados com sucesso!")
  return(gastos_educacao)
}
