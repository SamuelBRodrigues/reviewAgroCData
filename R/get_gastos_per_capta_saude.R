#' Extração dos dados de Gasto Per Capta com Saúde
#'
#' A função extra is dados de Gasto Per Capta com Saúde para os municípios direto
#' da plataforma da FGV Municípios
#'
#' @param cod_municipios_ibge Codigos do ibge para os muncípios
#'
#' @returns Uma tabela com os dados de Gasto Per Capta com Saúde para os anos disponíveis
#' @export
#'
#' @examples
#' \dontrun{
#'   get_gastos_per_capta_saude(cod_municipios_ibge = c("1508126", "1708205", "2101400"))
#'}
get_gastos_per_capta_saude <- function(cod_municipios_ibge = target_cities$municipio_codigo){

  purrr::map_df(
    cod_municipios_ibge,
    ~{
      # Simular o comportamento humano
      Sys.sleep(runif(1,1,3))
      # Url dos dados
      url <- "https://analitica.municipios.fgv.br/"

      # Parametros a serem enviados para API
      query_params <- list(
        callback = "jQuery364009964332437813739_1743094193524",
        api_ticket = "b09981dc947ed0b1e3a8371eeaf67178c5caf20ee90bcea702cb8c2a19145d3a",
        channel = "default",
        api_indicator = "f7664060cc52bc6f3d620bcedc94a4b6",
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
        )

      # Fazendo a requisição para API
      resp <- req |> httr2::req_perform()

      # Lendo a resposta da API como um texto
      body <- resp |> httr2::resp_body_string()

      # Estruturar a resposta como um json
      jsonp_response <- body

      # Remover o callback e extrair apenas o JSON
      json_text <- sub("^[^(]*\\((.*)\\);?$", "\\1", jsonp_response)

      # Converter JSON para lista
      json_data <- jsonlite::fromJSON(json_text)

      # Estruturando os dados em uma tabela
      data <- json_data$dados |>
        tibble::tibble() |>
        select(-DS_CONFIG) |>
        dplyr::mutate(
          ANO = as.character(ANO),
          municipio_codigo = .x
        ) |>
        janitor::clean_names() |>
        dplyr::rename(
          "municipio" = no_municipio,
          "valor_percapta_saude" = valor_percapta
        )

      message(stringr::str_glue("Pegando os dados: {data$municipio[1]}"))

      return(data)
    }
  )

}
