#' Pega Indicador de Conectividade Rural (ICR) por Estado
#'
#' Esta função acessa o site da ConectarAgro e baixa os indicadores de conectividade rural
#' para o(s) estado(s) selecionado(s). Se nenhum estado for especificado, a função retorna dados para todos os estados brasileiros.
#'
#' @param sigla_uf Character vector. Sigla(s) dos estados (UFs) a serem consultados.
#' Se NULL (default), busca dados para todos os estados ("AC", "AL", ..., "TO").
#'
#' @return Um tibble contendo os dados de conectividade rural para os municípios do(s) estado(s) informado(s).
#' As colunas retornadas dependem da estrutura do JSON fornecido pelo site.
#'
#' @examples
#' \dontrun{
#' # Buscar dados apenas do Acre
#' pega_conectividade_rural("AC")
#'
#' # Buscar dados de todos os estados
#' pega_conectividade_rural()
#' }
#'
#' @export
get_conectividade_rural <- function(sigla_uf = NULL) {

  if (is.null(sigla_uf)) {
    sigla_uf <- c(
      "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
      "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ",
      "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"
    )
  }

  conectividade_rural <- function(uf) {
    url <- stringr::str_glue(
      'https://www.conectaragro.com.br/indicadordeconectividaderural/json/{uf}.json'
    )

    jsonlite::read_json(url) %>%
      dplyr::tibble(data = .) %>%
      tidyr::unnest_wider(data)
  }

  icr_mun <- purrr::map(sigla_uf, conectividade_rural) %>%
    purrr::list_rbind() |>
    dplyr::rename(
      municipio_nome = NM_MUN,
      estado_sigla = SIGLA_UF
    )

  return(icr_mun)
}
