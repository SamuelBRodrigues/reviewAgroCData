#' Salário médio mensal dos trabalhadores formais
#'
#' A função extrai o Salário médio mensal dos trabalhadores formais diretamente
#' da API do IBGE.
#'
#' @param cod_ibge Vetor com os códigos dos municípios do IBGE.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
get_sal_med_mun <- function(cod_ibge){

  reqs <- purrr::map(
    cod_ibge,
    ~{
      url <- stringr::str_glue("https://servicodados.ibge.gov.br/api/v1/pesquisas/indicadores/143558/resultados/{.x}/")

      req <- httr2::request(url) |> httr2::req_retry(5) |> httr2::req_throttle(60, realm = "https://servicodados.ibge.gov.br/api/v1/")
    }
  )

  resps <- httr2::req_perform_parallel(
    reqs = reqs,
    max_active = 10,
    progress = T
  )

  extract_function <- function(resp){
    json <- resp |> httr2::resp_body_json()

    data <- tibble::tibble(
      codigo_ibge = json[[1]]$res[[1]]$localidade,
      ano = names(json[[1]]$res[[1]]$res),
      sal_med_mun = as.numeric(json[[1]]$res[[1]]$res$`2022`)
    ) |>
      dplyr::left_join(
        pop_municipios |>
          tidyr::unite(
            "cod_ibge",
            cod_uf:cod_munic,
            sep = ""
          ) |>
          dplyr::select(cod_ibge) |>
          dplyr::mutate(
            codigo_ibge = stringr::str_extract(cod_ibge, "......")
          )
      ) |>
      dplyr::select(-codigo_ibge)
  }

  dataset <- httr2::resps_data(
    resps,
    \(resp) extract_function(resp)
  )

}
