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
      req <- request(url) |>
        req_url_query(!!!query_params) |>
        req_headers(
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
      resp <- req |> req_perform()

      # Lendo a resposta da API como um texto
      body <- resp |> resp_body_string()

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
          ANO = as.character(ANO)
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


#' Extração dados de Mortalidade Infantil e Nascimento DATASUS
#'
#' A função extrai os dados de mortalidade infantil e nascimento direto da plataforma
#' do DATASUS através do pacote datasus e calcula a taxa de mortalidade infantil para
#' 1000 nascimentos a nível municipal.
#'
#' @param ano Ano de interesse dos dados
#'
#' @returns Um dataframe com os dados de Mortalidade Infantil, Nascimento, Taxa
#' de Mortaliade Infantil para 1000 nascimentos a nível municipal
#' @export
#'
#' @examples
#' \dontrun{
#'   get_taxa_mortalidade_infantil(ano = "2023")
#' }
get_taxa_mortalidade_infantil <- function(ano = "2022"){

  data_mortalidade <- datasus::sim_inf10_mun(periodo = ano) |>
    tibble::tibble() |>
    tail(-1) |>
    dplyr::mutate(
      cod_uf = stringr::str_extract(Município, ".."),
      Município = stringr::str_extract(Município, '\\D+$') |> stringr::str_remove(" ")
    ) |>
    subset(
      !stringr::str_detect(Município, 'IGNORADO')
    ) |>
    dplyr::left_join(
      pop_municipios |>
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = "",
          remove = FALSE
        ) |>
        dplyr::mutate(
          nome_do_municipio = stringr::str_to_upper(abjutils::rm_accent(nome_do_municipio)),
          municipio_codigo = as.character(municipio_codigo),
          cod_uf = as.character(cod_uf)
        ) |>
        dplyr::select(
          nome_do_municipio, municipio_codigo, cod_uf
        ),
      by = join_by(Município == nome_do_municipio, cod_uf == cod_uf)
    ) |>
    dplyr::rename(
      "mortalidade_infantil" = `Óbitos p/Residênc`
    ) |>
    dplyr::select(
      municipio_codigo, mortalidade_infantil, cod_uf
    )

  data_nascimento <- datasus::sinasc_nv_mun(periodo = ano) |>
    tibble::tibble() |>
    tail(-1) |>
    dplyr::mutate(
      cod_uf = stringr::str_extract(Município, ".."),
      Município = stringr::str_extract(Município, '\\D+$') |> stringr::str_remove(" ")
    ) |>
    subset(
      !stringr::str_detect(Município, 'IGNORADO')
    ) |>
    dplyr::left_join(
      pop_municipios |>
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = "",
          remove = FALSE
        ) |>
        dplyr::mutate(
          nome_do_municipio = stringr::str_to_upper(abjutils::rm_accent(nome_do_municipio)),
          municipio_codigo = as.character(municipio_codigo),
          cod_uf = as.character(cod_uf)
        ) |>
        dplyr::select(
          nome_do_municipio, municipio_codigo, cod_uf
        ),
      by = join_by(Município == nome_do_municipio, cod_uf == cod_uf)
    ) |>
    dplyr::rename(
      "nascimentos" = `Nascim p/resid.mãe`
    ) |>
    dplyr::select(
      municipio_codigo, nascimentos, cod_uf
    )

  data <- data_nascimento |>
    dplyr::left_join(
      data_mortalidade,
      by = join_by(municipio_codigo, cod_uf)
    ) |>
    dplyr::mutate(
      nascimentos = tidyr::replace_na(nascimentos, 0),
      mortalidade_infantil = tidyr::replace_na(mortalidade_infantil, 0),
      taxa_mortalidade_infantil = mortalidade_infantil/nascimentos * 1000
    )

  return(data)

}

#' Extração dados de Óbitos por Causas Evitáveis DATASUS
#'
#' A função extrai os dados óbitos por Causas evitáveis em menores do que 5 anos e
#' óbitos por Causas evitáveis entre 5 a 75 anos direto da plataforma do DATASUS
#' através do pacote datasus a nível municipal.
#'
#' @param ano Ano de interesse dos dados
#'
#' @returns Um dataframe com os dados óbitos evitáveis e as causas evitáveis a nível municipal
#' @export
#'
#' @examples
#' \dontrun{
#'   get_obitos_evitaveis(ano = "2023")
#' }
get_obitos_evitaveis <- function(ano = 2023){

  # Pegando os dados de morte evitaveis para menor do que 5 anos
  mortes_evitaveis_menor_5 <- datasus::sim_evita10_mun(coluna = "Causas evitáveis",periodo = ano)

  # Pegando os dados de morte evitaveis para maior do que 5 anos
  mortes_evitaveis_maior_5 <- datasus::sim_evitb10_mun(coluna = "Causas evitáveis",periodo = ano)

  # Estruturando os dados
  data <- mortes_evitaveis_menor_5 |>
    # Renomeando o valor total de acordo com cada base
    dplyr::rename(
      "Total_menor_5" = Total
    ) |>
    dplyr::left_join(
      mortes_evitaveis_maior_5 |>
        dplyr::rename(
          "Total_maior_5" = Total
        ),
      by = join_by(Município),
      suffix = c("_menor_5", "_maior_5")
    ) |>
    # Transformando em um tibble
    tibble::tibble() |>
    # Retirando as linhas "MUNICIPIO IGNORADO"
    subset(
      !stringr::str_detect(Município, "MUNICIPIO IGNORADO")
    ) |>
    # Removendo a soma total
    tail(-1) |>
    # Pegando o codigo das UF e o nome dos municípios
    dplyr::mutate(
      cod_uf = stringr::str_extract(Município, ".."),
      Município = stringr::str_extract(Município, '\\D+$') |> stringr::str_remove(" ")
    ) |>
    # Renomeando a coluna município
    dplyr::rename(
      "nome_do_municipio" = Município
    ) |>
    # Adicionando os códigos do ibge dos municípios
    dplyr::left_join(
      pop_municipios |>
        # Criando a coluna com os códigos do ibge dos municípios
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = "",
          remove = FALSE
        ) |>
        # Corrigindo nomes dos município
        dplyr::mutate(
          nome_do_municipio = stringr::str_to_upper(abjutils::rm_accent(nome_do_municipio)),
          municipio_codigo = as.character(municipio_codigo),
          cod_uf = as.character(cod_uf)
        ) |>
        # Selecionando as colunas de interesse
        dplyr::select(
          nome_do_municipio, municipio_codigo, cod_uf
        ),
    ) |>
    # Tratando os NA
    dplyr::mutate(
      dplyr::across(-c(nome_do_municipio, municipio_codigo),
                    ~ tidyr::replace_na(.x, 0))
    ) |>
    # Removendo colunas que não são de interesse
    dplyr::select(
      -nome_do_municipio, -cod_uf
    ) |>
    dplyr::mutate(
      ano = ano
    ) |>
    dplyr::relocate(
      municipio_codigo,
      ano
    ) |>
    janitor::clean_names()

}

#' Extração dos dados de Abastecimento de Água e Esgotamento Sanitário
#'
#' A função extrai os dados de Abastecimento de Água e Esgotamento Sanitário a
#' nível municipal para o ano de 2021 diretamente da plataforma do Instituto Água
#' e Saneamento.
#'
#' @returns Uma dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data_abatecimento_e_esgoto <- get_abastecimento_esgoto()
#' }
get_abastecimento_esgoto <- function(){

  # Construindo a requisição para acessar os dados de Abastecimento e Esgoto dos
  # Municípios

  req <- httr2::request("https://www.aguaesaneamento.org.br/municipios-e-saneamento/exploreCustomCSV") |>
    httr2::req_url_query(
      v2 = "",
    ) |>
    httr2::req_headers(
      accept = "text/plain, */*; q=0.01",
      `accept-language` = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
      `content-type` = "application/x-www-form-urlencoded; charset=UTF-8",
      cookie = "_gid=GA1.3.1944777815.1743187811; _ga=GA1.1.937080257.1743187811; _hjSessionUser_1925616=eyJpZCI6IjIzOWJjNzk1LTg3MGEtNWEyMi1iNmIxLWEyYmM1NDk0ZDMwYSIsImNyZWF0ZWQiOjE3NDMxODc4MTUzNzcsImV4aXN0aW5nIjp0cnVlfQ==; _hjSession_1925616=eyJpZCI6ImZjYjQxZTA1LTc4ZTEtNDdiNS1hMmYwLTQyYTk1NjVhNDU2ZiIsImMiOjE3NDMxODc4MTUzODAsInMiOjEsInIiOjEsInNiIjowLCJzciI6MCwic2UiOjAsImZzIjoxLCJzcCI6MH0=; _ga_GTTPTZB4XM=GS1.1.1743187814.1.1.1743189223.2.0.1817436974",
      origin = "https://www.aguaesaneamento.org.br",
      priority = "u=1, i",
      `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36 OPR/117.0.0.0",
      `x-requested-with` = "XMLHttpRequest",
    ) |>
    httr2::req_body_raw("filtro=cidade||asc&estado=&bioma=&faixa=&politica=&agrupamento=&snis=&rm=&indicador=2|3|&aglom=&ride=", "application/x-www-form-urlencoded; charset=UTF-8")

  # Performando a requisição
  resp <- httr2::req_perform(req)

  # Estruturando a resposta em uma tabela csv2
  csv_table <- resp |>
    httr2::resp_body_string() |>
    readr::read_csv2()

  # Tratando e estruturando os dados
  data <- tabela |>
    janitor::clean_names() |>
    dplyr::select(
      codigo_ibge, cidade,
      indice_da_populacao_total_atendida_com_esgotamento_sanitario_percent,
      indice_da_populacao_total_atendida_com_abastecimento_de_agua_percent
    ) |>
    dplyr::rename(
      "municipio_codigo" = codigo_ibge,
      "municipio_nome" = cidade
    ) |>
    dplyr::mutate(
      ano = "2021"
    )

  return(data)
}

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

  # Construindo a requisição para API
  req <- httr2::request(url) |>
    httr2::req_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
      `Accept-Language` = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
      `Cache-Control` = "max-age=0",
      Cookie = "BIGipServeregestorab_prod=1946230188.8225.0000; _gid=GA1.3.1498056456.1743187813; JSESSIONID=pNkjqgczwwfoMR5KDlEDX-h-; _ga_PQPQJHMHNQ=GS1.1.1743193251.1.0.1743193258.0.0.0; _ga=GA1.1.1753857876.1743187813; _ga_YG7RPWS2GW=GS1.1.1743191385.2.1.1743193922.0.0.0",
      Origin = "https://egestorab.saude.gov.br",
      `Upgrade-Insecure-Requests` = "1",
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36 OPR/117.0.0.0",
    ) |>
    httr2::req_body_raw(stringr::str_glue("j_idt58=j_idt58&javax.faces.ViewState=8824599833687260399%3A8438076036273788615&j_idt58%3AtipoConsulta=uniGeoComp&j_idt58%3AunidGeo=municipio&j_idt58%3Aregiao=00&j_idt58%3Aestados=00&j_idt58%3Amunicipios=00&j_idt58%3AcompInicio={ano}{mes}&j_idt58%3AverTela=Ver+em+tela&example_length=10"), "application/x-www-form-urlencoded")

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

#' Extrai os dados de Cobertura Vacinal
#'
#' A função extrai os dados de Cobertura Vacinal a nível municipal diretamente do
#' PowerBI Índice de Governança Municipal do Conselho Federal de Administração,
#' IGM - CFA.
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
                              Values = list(list(list(Literal = list(Value = "2024L"))))
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
        cobertura_vacinal_municipio = json[["results"]][[1]][["result"]][["data"]][["dsr"]][["DS"]][[1]][["PH"]][[1]][["DM0"]][[1]][["C"]][[1]],
        cobertura_vacinal_max = json[["results"]][[1]][["result"]][["data"]][["dsr"]][["DS"]][[1]][["PH"]][[1]][["DM0"]][[1]][["C"]][[2]]
      )

      data |> dplyr::glimpse()
      return(data)

    }
  )
  return(data_table)
}
