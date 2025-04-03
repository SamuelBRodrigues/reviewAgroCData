#' Extração dos dados de Cobertura de Atenção Primária a Saúde (APS)
#'
#' A função extrai os dados de Cobertura APS diretamente da plataforma e-Gestor
#' do ministério da Saúde. Os dados são detalhados a nível municipal para o ano
#' e mês inserido
#'
#' @param ano Ano de interesse dos dados. Por padrão usa o ano de 2024
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
get_cobertura_aps <- function(ano = 2024){

  if(ano == 2024){
    data <- reviewAgroCData::cobertura_aps_04_2024
  } else if(ano == 2023){
    data <- reviewAgroCData::cobertura_aps_12_2023
  } else{
    stop("Anos diponíveis apenas para 2023 e 2024")
  }

  # Tratando a tabela para os dados de interesse
  data <- data |>
    janitor::clean_names() |>
    dplyr::select(
      competencia_cnes, ibge, municipio, cobertura_aps
    ) |>
    dplyr::rename(
      "time_period" = competencia_cnes
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
    ) |>
    dplyr::select(
      municipio_codigo, municipio, time_period, cobertura_aps
    )

  return(data)
}
