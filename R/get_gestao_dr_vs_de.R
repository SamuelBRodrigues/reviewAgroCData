#' Obtém o indicador Gestão (Dispêndio do Recurso vs Desempenho - Educação)
#'
#' @param ano Ano de referência (padrão: 2023).
#'
#' @return Um data frame contendo o código do município, nome, estado e o índice de gestão de recursos vs desempenho educacional.
#'
#' @importFrom dplyr select inner_join mutate rename across
#' @importFrom glue glue
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   get_gestao_dr_vs_de(2023)
#' }
#'
#' @export
get_gestao_dr_vs_de <- function(ano = 2023) {
  # Obter dados do IDEB
  ideb_anos_iniciais <- get_IDEB_fundamental(download = FALSE, ano = ano, tipo = "anos_iniciais") %>%
    dplyr::select(municipio_codigo,
                  municipio_nome,
                  estado_sigla,
                  !!glue::glue("ideb_{ano}_anos_iniciais"))

  ideb_anos_finais <- get_IDEB_fundamental(download = FALSE, ano = ano, tipo = "anos_finais") %>%
    dplyr::select(municipio_codigo, !!glue::glue("ideb_{ano}_anos_finais"))

  # Carregar dados de despesas educacionais
  file_path <- glue::glue("data/despesas_educacao/despesas_educacao_{ano}.RData")
  if (!file.exists(file_path)) {
    stop(glue::glue("Arquivo {file_path} não encontrado. Verifique o caminho."))
  }

  despesas_educacao <- get(load(file_path)) %>%
    dplyr::rename(municipio_codigo = `Cod.IBGE`,
                  !!glue::glue("despesas_educacao_{ano}") := `Despesas Liquidadas`) %>%
    dplyr::mutate(municipio_codigo = as.character(municipio_codigo)) %>%
    dplyr::select(municipio_codigo, !!glue::glue("despesas_educacao_{ano}"))

  # Definir nomes das colunas dinamicamente
  ideb_iniciais_col <- glue::glue("ideb_{ano}_anos_iniciais")
  ideb_finais_col   <- glue::glue("ideb_{ano}_anos_finais")
  despesas_col      <- glue::glue("despesas_educacao_{ano}")
  gestao_dr_vs_de_col <- glue::glue("gestao_dr_vs_de_{ano}")

  # Unir os datasets e calcular o índice de gestão
  gestao_dr_vs_de <- ideb_anos_iniciais %>%
    dplyr::inner_join(ideb_anos_finais, by = "municipio_codigo") %>%
    dplyr::inner_join(despesas_educacao, by = "municipio_codigo") %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c(ideb_iniciais_col, ideb_finais_col, despesas_col)), as.double),
      !!gestao_dr_vs_de_col := .data[[despesas_col]] /
        (.data[[ideb_iniciais_col]] + .data[[ideb_finais_col]])
    )

  return(gestao_dr_vs_de)
}
