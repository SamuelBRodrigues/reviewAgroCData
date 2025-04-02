#' Extrai os dados de Cobertura Vacinal
#'
#' A função extrai os dados de Cobertura Vacinal a nível municipal diretamente do
#' PowerBI Índice de Governança Municipal do Conselho Federal de Administração,
#' IGM - CFA.
#'
#' @param cod_ibge Vetor character com os códigos do ibge dos municípios. Por padrão
#' utiliza os códigos dos municípios da tabela target_cities
#' @param ano Ano de interesse dos dados. Por padrão usa 2024.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'  data_cobertura_vacinal <- get_cobertura_vacinal(
#'   cod_ibge = c("1508126", "1708205", "2101400"),
#'   ano = 2024
#'  )
#' }
get_cobertura_vacinal <- function(cod_ibge = target_cities$municipio_codigo,
                                  ano = 2024){

  # Estruturando os dados para que possam ser reconhecidos pelo arg da função
  table_municipio <- pop_municipios |>
    tidyr::unite(
      "municipio_codigo",
      cod_uf:cod_munic,
      sep = ""
    ) |>
    dplyr::mutate(
      nome_do_municipio = stringr::str_to_upper(abjutils::rm_accent(nome_do_municipio)),
      municipio = stringr::str_glue("{nome_do_municipio} - {uf}")
    )
  # Criando uma named list
  data_cidade <- setNames(table_municipio$municipio,table_municipio$municipio_codigo)

  data_table <- purrr::map_df(
    cod_ibge,
    ~{
      # Mimetizando comportamento humano
      Sys.sleep(runif(1,1,3))
      # Selecionando a cidade
      cidade <- data_cidade[.x]

      # Definir a URL da requisição
      url <- "https://wabi-brazil-south-api.analysis.windows.net/public/reports/querydata?synchronous=true"

      # Definir os headers da requisição
      headers <- list(
        "Accept" = "application/json, text/plain, */*",
        "Accept-Language" = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
        "ActivityId" = "2873a0f7-61d0-4954-beb1-3fe8f012e2c8",
        "Connection" = "keep-alive",
        "Content-Type" = "application/json;charset=UTF-8",
        "Origin" = "https://app.powerbi.com",
        "Referer" = "https://app.powerbi.com/",
        "RequestId" = "deddd3a3-6b62-81b4-ade7-ab1cb090d898",
        "Sec-Fetch-Dest" = "empty",
        "Sec-Fetch-Mode" = "cors",
        "Sec-Fetch-Site" = "cross-site",
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36 OPR/117.0.0.0",
        "X-PowerBI-ResourceKey" = "f5adf4d1-ab3e-4370-ae6e-2008edc90875",
        "sec-ch-ua" = "\"Not A(Brand\";v=\"8\", \"Chromium\";v=\"132\", \"Opera GX\";v=\"117\"",
        "sec-ch-ua-mobile" = "?0",
        "sec-ch-ua-platform" = "\"Windows\""
      )

      # Definir o corpo da requisição
      body <- list(
        version = "1.0.0",
        queries = list(
          list(
            Query = list(
              Commands = list(
                list(
                  SemanticQueryDataShapeCommand = list(
                    Query = list(
                      Version = 2,
                      From = list(
                        list(Name = "i", Entity = "IGM CFA", Type = 0)
                      ),
                      Select = list(
                        list(
                          Aggregation = list(
                            Expression = list(
                              Column = list(
                                Expression = list(SourceRef = list(Source = "i")),
                                Property = "Desempenho - Saúde - Cobertura Vacinal - Dado Bruto"
                              )
                            ),
                            Function = 0
                          ),
                          Name = "Sum(IGM CFA.Desempenho - Saúde - Cobertura Vacinal - Dado Bruto)"
                        ),
                        list(
                          Aggregation = list(
                            Expression = list(
                              Column = list(
                                Expression = list(SourceRef = list(Source = "i")),
                                Property = "Desempenho - Saúde - Cobertura Vacinal - Meta"
                              )
                            ),
                            Function = 0
                          ),
                          Name = "Sum(IGM CFA.Desempenho - Saúde - Cobertura Vacinal - Meta)",
                          NativeReferenceName = "Desempenho - Saúde - Cobertura Vacinal - Meta"
                        )
                      ),
                      Where = list(
                        list(
                          Condition = list(
                            In = list(
                              Expressions = list(
                                list(Column = list(Expression = list(SourceRef = list(Source = "i")), Property = "ano"))
                              ),
                              Values = list(list(list(Literal = list(Value = stringr::str_glue("{ano}L")))))
                            )
                          )
                        ),
                        list(
                          Condition = list(
                            In = list(
                              Expressions = list(
                                list(Column = list(Expression = list(SourceRef = list(Source = "i")), Property = "Nome_UF"))
                              ),
                              Values = list(list(list(Literal = list(Value = stringr::str_glue("'{cidade}'")))))
                            )
                          )
                        )
                      )
                    ),
                    Binding = list(
                      Primary = list(Groupings = list(list(Projections = c(0, 1)))),
                      Version = 1
                    ),
                    ExecutionMetricsKind = 1
                  )
                )
              )
            ),
            QueryId = "",
            ApplicationContext = list(
              DatasetId = "35860f86-188c-4360-a50f-3febbb08041c",
              Sources = list(
                list(ReportId = "0ba0c1e5-15df-49a2-bdde-de130f8c6a4a", VisualId = "772ad347d2c0b7a34aea")
              )
            )
          )
        ),
        cancelQueries = list(),
        modelId = 7655674
      )

      # Fazer a requisição
      resp <- httr2::request(url) %>%
        httr2::req_headers(!!!headers) %>%
        httr2::req_body_json(body) %>%
        httr2::req_perform()

      # Ver a resposta como um json
      json <- resp |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        jsonlite::toJSON() |>
        jsonlite::parse_json()


      data <- tibble::tibble(
        municipio_codigo = .x,
        municipio = cidade,
        ano = ano,
        cobertura_vacinal_municipio = json[["results"]][[1]][["result"]][["data"]][["dsr"]][["DS"]][[1]][["PH"]][[1]][["DM0"]][[1]][["C"]][[1]],
        cobertura_vacinal_max = json[["results"]][[1]][["result"]][["data"]][["dsr"]][["DS"]][[1]][["PH"]][[1]][["DM0"]][[1]][["C"]][[2]]
      )

      data |> dplyr::glimpse()
      return(data)

    }
  )
  return(data_table)
}
