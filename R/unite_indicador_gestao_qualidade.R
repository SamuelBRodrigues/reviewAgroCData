#' Une as tabelas que compõe o indicador de saúde
#'
#' Essa função une as tabelas que compões o indicador de saúde em um único dataframe
#'
#' @param endividamento Tabela com os dados de Endividamento
#' @param poupanca_corrente Tabela com os dados de Poupança Corrente
#' @param liquidez Tabela com os dados de Liquidez
#'
#' @returns Dataframe com todas as variáveis que compõe o indicador de saúde
#' @export
#'
#' @examples
unite_indicador_gestao_qualidade <- function(
    endividamento, poupanca_corrente, liquidez
){
  data <- target_cities |>
    dplyr::select(-populacao) |>
    dplyr::left_join(
      endividamento |>
        dplyr::select(
          municipio_codigo, endividamento_2023
        )
    ) |>
    dplyr::left_join(
      poupanca_corrente
    ) |>
    dplyr::select(
      municipio_codigo, poupanca_corrente_2023
    ) |>
    dplyr::left_join(
      liquidez |>
        dplyr::select(
          municipio_codigo, liquidez_2023
        )
    )
}
