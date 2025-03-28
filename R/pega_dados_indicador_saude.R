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



