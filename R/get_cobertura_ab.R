#' Extração dos dados de Cobertura de Atenção Básica
#'
#' A função extrai os dados de Cobertura de Atenção Básica diretamente da plataforma
#' e-Gestor do ministério da Saúde. Os dados são detalhados a nível municipal para
#' o ano indicado.
#'
#' @param ano Ano de interesse dos dados. Por padrão usa o ano de 2020
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   dados_cobertura_ab <- get_cobertura_ab(
#'     ano = "2020"
#'   )
#' }
get_cobertura_ab <- function(ano = 2020){

  if(ano == 2020){
    data <- reviewAgroCData::cobertura_ab_12_2020
  } else if(ano == 2019){
    data <- reviewAgroCData::cobertura_ab_12_2019
  } else{
    stop("Anos diponíveis apenas para 2019 e 2020")
  }

  # Tratando a tabela para os dados de interesse
  data <- data |>
    janitor::clean_names() |>
    dplyr::select(
      ibge, municipio, cobertura_ab
    ) |>
    dplyr::rename(
      !!stringr::str_glue("cobertura_ab_{ano}") := cobertura_ab
    ) |>
    dplyr::left_join(
      pop_municipios |>
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = ""
        ) |>
        dplyr::select(municipio_codigo) |>
        dplyr::mutate(
          ibge = stringr::str_extract(municipio_codigo, "^.*(?=.$)")
        )
    )  |>
    dplyr::select(
      -ibge
    ) |>
    dplyr::relocate(
      municipio_codigo, municipio
    )

  return(data)
}
