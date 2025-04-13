#' Pega os dados de todos os indicadores
#'
#' Essa função é responsável por pegar os dados de todos os indicadores e unir todos
#' em uma única tabela
#'
#' @param cod_ibge Vetor com códigos IBGE. Padrão: target_cities$municipio_codigo
#' @param ano Ano de referência. Padrão: Mais recente ("last")
#' @param mes Mês de referência. Padrão: Mais recente ("last")
#' @param download Lógico. Se TRUE, baixa os dados. Padrão: TRUE
#' @param dir  Diretório dos dados CRAS. Padrão: "data_raw"
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- pega_dados_indicadores(
#'     cod_ibge = target_cities$municipio_codigo, ano = "last", mes = "06", download = TRUE,
#'     dir = "data_raw"
#'   )
#' }
pega_dados_indicadores <- function(
    cod_ibge = target_cities$municipio_codigo, ano = "last", mes = "03", download = TRUE,
    dir = "data_raw"
    ){

  dados_indicador_saude <- pega_dados_indicador_saude(cod_ibge = cod_ibge, ano = ano)
  dados_indicador_educacao <- pega_dados_indicador_educacao(cod_ibge = cod_ibge, ano = ano, download = download)
  dados_indicador_protecao_social <- pega_dados_indicador_protecao_social(cod_municipios_ibge = cod_ibge,
                                                          ano = ano,
                                                          mes = mes,
                                                          dir = dir)
  dados_indicador_mobilidade <- pega_dados_indicador_mobilidade(ano = ano)
  dados_indicador_gestao_qualidade <- pega_dados_indicador_gestao_qualidade(ano = ano, download = download)
  dados_indicador_desenvolvimento_economico <- pega_dados_indicador_desenvolvimento_economico(ano = ano)

  data <- dados_indicador_educacao |>
    dplyr::left_join(
      dados_indicador_saude
    ) |>
    dplyr::left_join(
      dados_indicador_desenvolvimento_economico
    ) |>
    dplyr::left_join(
      dados_indicador_gestao_qualidade
    )  |>
    dplyr::left_join(
      dados_indicador_mobilidade
    )  |>
    dplyr::left_join(
      dados_indicador_protecao_social |>
        dplyr::select(-estado_sigla)
    )
  return(data)
}
