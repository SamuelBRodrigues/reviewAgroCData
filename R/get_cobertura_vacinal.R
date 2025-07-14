#' Extrai os dados de Cobertura Vacinal
#'
#' A função extrai os dados de Cobertura Vacinal a nível municipal diretamente
#' da plataforma da FGV Municípios
#'
#' @param cod_ibge Vetor character com os códigos do ibge dos municípios. Por padrão
#' utiliza os códigos dos municípios da tabela target_cities
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'  data_cobertura_vacinal <- get_cobertura_vacinal(
#'   cod_ibge = c("1508126", "1708205", "2101400")
#'  )
#' }
get_cobertura_vacinal <- function(cod_ibge = target_cities$municipio_codigo){

  reqs <- purrr::map(
    cod_ibge,
    ~ {

      cod_munic <- .x
      # url da API
      url <- "https://analitica.municipios.fgv.br/"

      req <- httr2::request(url) |>
        httr2::req_url_query(
          callback = "jQuery364011825524496822326_1750829718859",
          api_ticket = "b09981dc947ed0b1e3a8371eeaf67178c5caf20ee90bcea702cb8c2a19145d3a",
          channel = "default",
          api_indicator = "19f3cd308f1455b3fa09a282e0d496f4", # Codigo indicador: Cobertura Vacinal
          filters = stringr::str_glue("co_municipio,2,'{cod_munic}'") # Filtro para o código do município
        ) |>
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
        httr2::req_throttle(60, realm = url) # Limite de requisições por minuto
    }
  )

  resps <- httr2::req_perform_parallel(reqs,
                                       max_active = 10, # Limite de requisições em paralelo
                                       progress = TRUE
  )

  extract_function <- function(resp){
    # Pegando o código do município
    cod_munic <- resp$request$url |>
      stringr::str_extract("(?<=27)[0-9]{7}(?=%)") |>
      as.integer()

    # Pegando o body da resposta
    body <- resp |> httr2::resp_body_string()

    # Convertendo o body em um JSON
    json <- stringr::str_match(body, "\\((.*)\\)")[, 2] |>
      jsonlite::fromJSON()

    # Estruturando os dados em uma tabela
    data <- json$dados |>
      tibble::tibble() |>
      dplyr::mutate(
        municipio_codigo = cod_munic
      )

  }

  dataset <- httr2::resps_data(
    resps,
    \(resp) extract_function(resp)
  )

  data_treated <- dataset |>
    dplyr::select(municipio_codigo, NO_MUNICIPIO, ANO, NR_COBERTRA_VACINAL_2010) |>
    dplyr::mutate(
      ANO = as.integer(ANO)
    ) |>
    dplyr::rename(
      municipio_nome = NO_MUNICIPIO,
      ano = ANO,
      cobertura_vacinal = NR_COBERTRA_VACINAL_2010
    )

  null_mun <- setdiff(cod_ibge, data_treated$municipio_codigo)

  if(length(null_mun) > 0){
    warning(
      "Alguns municípios não retornaram dados: ",
      paste(null_mun, collapse = ", ")
    )
  }

  return(data_treated)
}
