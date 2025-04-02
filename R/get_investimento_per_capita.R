#' Extrai os dados da nota quanto a Investimento Per Capita do CFA
#'
#' A função extrai os dados da nota quanto a Investimento Per Capita do CFA a
#' nível municipal diretamente do PowerBI Índice de Governança Municipal do Conselho
#' Federal de Administração, IGM - CFA.
#'
#' @param cod_ibge Vetor character com os códigos do ibge dos municípios. Por padrão
#' utiliza os códigos dos municípios da tabela target_cities
#' @param ano Ano de interesse dos dados. Por padrão usa o ano de 2024.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'  data_investimento_per_capita <- get_cobertura_vacinal(
#'   cod_ibge = c("1508126", "1708205", "2101400"),
#'   ano = 2024
#'  )
#' }
get_investimento_per_capita <- function(cod_ibge = target_cities$municipio_codigo, ano = 2024){

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

  data_investimento_per_capita <- purrr::map_df(
    cod_ibge,
    ~{
      # Mimetizando comportamento humano
      Sys.sleep(runif(1,1,3))
      # Selecionando a cidade
      cidade <- data_cidade[.x]

      # Definir a URL da requisição
      url <- "https://wabi-brazil-south-api.analysis.windows.net/public/reports/querydata?synchronous=true"

      # Criar o corpo da requisição como uma lista
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
                      From = list(list(Name = "i", Entity = "IGM CFA", Type = 0)),
                      Select = list(
                        list(
                          Aggregation = list(
                            Expression = list(
                              Column = list(
                                Expression = list(SourceRef = list(Source = "i")),
                                Property = "Finanças - Investimento per Capita (Indicador)"
                              )
                            ),
                            Function = 0
                          ),
                          Name = "Sum(IGM CFA.Finanças - Investimento per Capita (Indicador))"
                        )
                      ),
                      Where = list(
                        list(
                          Condition = list(
                            In = list(
                              Expressions = list(
                                list(Column = list(
                                  Expression = list(SourceRef = list(Source = "i")),
                                  Property = "ano"
                                ))
                              ),
                              Values = list(list(list(Literal = list(Value = stringr::str_glue("{ano}L")))))
                            )
                          )
                        ),
                        list(
                          Condition = list(
                            In = list(
                              Expressions = list(
                                list(Column = list(
                                  Expression = list(SourceRef = list(Source = "i")),
                                  Property = "Nome_UF"
                                ))
                              ),
                              Values = list(list(list(Literal = list(Value = stringr::str_glue("'{cidade}'")))))
                            )
                          )
                        )
                      )
                    ),
                    Binding = list(
                      Primary = list(Groupings = list(list(Projections = list(0)))),
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
                list(ReportId = "0ba0c1e5-15df-49a2-bdde-de130f8c6a4a", VisualId = "7145f91f57b9926609bb")
              )
            )
          )
        ),
        cancelQueries = list(),
        modelId = 7655674
      )

      # Criar e enviar a requisição
      res <- httr2::request(url) |>
        httr2::req_headers(
          "Accept" = "application/json, text/plain, */*",
          "Accept-Language" = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
          "ActivityId" = "2089e526-4d94-4c7f-b214-1b5e38e1086e",
          "Connection" = "keep-alive",
          "Content-Type" = "application/json;charset=UTF-8",
          "Origin" = "https://app.powerbi.com",
          "Referer" = "https://app.powerbi.com/",
          "RequestId" = "bebc9eaa-3bbe-0217-4837-28602289dc04",
          "Sec-Fetch-Dest" = "empty",
          "Sec-Fetch-Mode" = "cors",
          "Sec-Fetch-Site" = "cross-site",
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36 OPR/117.0.0.0",
          "X-PowerBI-ResourceKey" = "f5adf4d1-ab3e-4370-ae6e-2008edc90875",
          "sec-ch-ua" = "\"Not A(Brand\";v=\"8\", \"Chromium\";v=\"132\", \"Opera GX\";v=\"117\"",
          "sec-ch-ua-mobile" = "?0",
          "sec-ch-ua-platform" = "\"Windows\""
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_perform()

      # Ver a resposta como um json
      response_data <- res |> httr2::resp_body_string() |> jsonlite::fromJSON()

      # Nota de Investimento Per Capita CFA
      nota <- response_data$results$result$data$dsr$DS[[1]]$PH[[1]]$DM0[[1]]$M0
      # Nome da variável extraída
      variavel <- response_data$results$result$data$descriptor$Select[[1]]$Name

      # Estruturando dados em uma tabela
      data <- tibble::tibble(
        muncipio_codigo = .x,
        municipio = cidade,
        ano = ano,
        investimento_per_capita = nota,
        variavel = variavel
      )

      data |> dplyr::glimpse()
      return(data)

    }
  )
  return(data_investimento_per_capita)

}
