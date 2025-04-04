#' Tratamento dos dados de Empresas que permanecem permanecem abertas
#'
#' A função trata os dados de Empresas que permanecem abertas que foram extraídos
#' diretamente do Painel do Mapa de Empresas do gov.br
#'
#' @returns Um dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#'  data_empresas <- get_perc_empresas_permanecem_abertas()
#' }
get_perc_empresas_permanecem_abertas <- function(){

  data <- reviewAgroCData::data_empresas |>
    dplyr::mutate(
      perc_empresas_permanecem_abertas_2025 = (estabelecimentos_fechados_2025/estabelecimentos_abertos_2025)*100
    )
  return(data)
}
