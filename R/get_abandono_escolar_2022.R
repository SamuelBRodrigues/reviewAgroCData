#' Obtém os dados de abandono escolar de 2022
#'
#' Esta função baixa e extrai os dados de abandono escolar referentes ao ano de 2022.
#' Primeiro, realiza o download do arquivo ZIP contendo os dados, extrai o conteúdo e, em seguida, carrega e filtra as informações relevantes.
#'
#' @param download Lógico. Se `TRUE`, os dados serão baixados antes da extração. O padrão é `TRUE`.
#'
#' @return Um data frame contendo as taxas de abandono escolar para municípios selecionados, com as seguintes colunas:
#' - `municipio_codigo`: Código do município
#' - `municipio_nome`: Nome do município
#' - `estado_sigla`: Sigla do estado
#' - `taxa_abandono_ensino_fundamental`: Taxa de abandono no ensino fundamental
#' - `taxa_abandono_ensino_medio`: Taxa de abandono no ensino médio
#'
#' @details
#' Caso `download = TRUE`, a função chama `download_abandono_escolar()` para baixar os dados antes da extração. Em seguida, utiliza `extract_abandono_escolar_2022()` para processar e retornar as informações filtradas.
#'
#' @examples
#' dados <- get_abandono_escolar_2022()
#' @export
get_abandono_escolar_2022 <- function(download = TRUE) {
  if (download) {
    sucesso <- tryCatch(
      {
        download_abandono_escolar()
        TRUE  # Indica que o download foi bem-sucedido
      },
      error = function(e) {
        warning("Erro ao baixar os dados de abandono escolar: ", conditionMessage(e))
        FALSE  # Indica que houve erro no download
      }
    )

    if (!sucesso) {
      return(NULL)  # Se o download falhar, interrompe a função
    }
  }

  abandono_escolar_2022 <- extract_abandono_escolar_2022()

  return(abandono_escolar_2022)
}
