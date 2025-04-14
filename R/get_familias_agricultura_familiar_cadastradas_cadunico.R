#' Extrai os dados de Famílias de Agricultores Familiar que são Cadastrados no CadÚnico
#'
#' Essa função extrai os dados de Famílias de Agricultores Familiar que são Cadastrados
#' no CadÚnico diretamente da plataforma do Cidadania a nível municipal para um
#' determinado ano e mês.
#'
#' @param cod_ibge Vetor character com os códigos do ibge dos municípios. Por padrão
#' utiliza os códigos dos municípios da tabela target_cities
#' @param ano Ano de interesse dos dados. Por padrão usa "last" que se refere ao
#' ano dado mais recente.
#' @param mes Mês de interesse dos dados. O mês deve ser um character entre "01"
#' e "12"
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_familias_agricultura_familiar_cadastradas_cadunico(
#'     cod_ibge = c("1508126", "1708205", "2101400"),
#'     ano = 2024,
#'     mes = "03"
#'   )
#' }
get_familias_agricultura_familiar_cadastradas_cadunico <- function(
    cod_ibge = target_cities$municipio_codigo, ano = "last", mes = "01"
){
  if(ano == "last"){
    data <- purrr::map_df(
      cod_ibge,
      ~{
        # Url dos dados
        url <- "https://cecad.cidadania.gov.br/agregado/resumovariavelCecad.php"
        # Codigo do ibge do município
        cod_ibge <- .x
        # Adaptando o codigo do ibge do município para o input aceitável da API
        cod <- stringr::str_extract(cod_ibge, "......")
        # Fazendo a requisição para a API
        req <- httr2::request(url) |>
          httr2::req_url_query(
            oik = "1",
            id = "95",
            p_ibge = cod,
          )
        resp <- httr2::req_perform(req)

        # Estruturando a resposta da API
        resp_body_string <- resp |>
          httr2::resp_body_string()

        # Extrair a parte com os dados do arrayToDataTable
        dados_brutos <- stringr::str_extract(resp_body_string, "(?s)google\\.visualization\\.arrayToDataTable\\(\\[(.*?)\\]\\)")
        # Agora extrair só os dados (sem a primeira linha de cabeçalho)
        linhas <- stringr::str_match_all(dados_brutos, "\\['(\\d{2}/\\d{4})',(\\d+),'(\\d{4})',\\s*'(\\d{2})'\\]")[[1]]

        dados_df <- tibble::tibble(
          municipio_codigo = cod_ibge,
          year = as.integer(linhas[, 4]),
          month = linhas[, 5],
          familias = as.integer(linhas[, 3]),
        ) |>
          tail(1)

        y <- dados_df$year
        m <- dados_df$month

        # Estruturando os dados de interesse
        data <- tibble::tibble(
          municipio_codigo = dados_df$municipio_codigo,
          familias = dados_df$familias
        ) |>
          dplyr::rename(
            !!stringr::str_glue("pop_vulnerabilidade_rural_{m}_{y}") := familias
          )
      }
    )
  } else{
    data <- purrr::map_df(
      cod_ibge,
      ~{

        # Url dos dados
        url <- "https://cecad.cidadania.gov.br/agregado/resumovariavelCecad.php"
        # Codigo do ibge do município
        cod_ibge <- .x
        # Adaptando o codigo do ibge do município para o input aceitável da API
        cod <- stringr::str_extract(cod_ibge, "......")
        # Fazendo a requisição para a API
        req <- httr2::request(url) |>
          httr2::req_url_query(
            oik = "1",
            id = "95",
            p_ibge = cod,
          )
        resp <- httr2::req_perform(req)

        # Estruturando a resposta da API
        resp_body_string <- resp |>
          httr2::resp_body_string()

        # Extrair a parte com os dados do arrayToDataTable
        dados_brutos <- str_extract(resp_body_string, "(?s)google\\.visualization\\.arrayToDataTable\\(\\[(.*?)\\]\\)")
        # Agora extrair só os dados (sem a primeira linha de cabeçalho)
        linhas <- stringr::str_match_all(dados_brutos, "\\['(\\d{2}/\\d{4})',(\\d+),'(\\d{4})',\\s*'(\\d{2})'\\]")[[1]]

        dados_df <- tibble::tibble(
          municipio_codigo = cod_ibge,
          year = as.integer(linhas[, 4]),
          month = linhas[, 5],
          familias = as.integer(linhas[, 3]),
        ) |>
          dplyr::filter(
            year == ano, month == mes
          )

        y <- dados_df$year
        m <- dados_df$month

        # Estruturando os dados de interesse
        data <- tibble::tibble(
          municipio_codigo = dados_df$municipio_codigo,
          familias = dados_df$familias
        ) |>
          dplyr::rename(
            !!stringr::str_glue("pop_vulnerabilidade_rural_{m}_{y}") := familias
          )

      }
    )
  }

}
