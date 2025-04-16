#' Constroi os dados de Obesidade Municipal - Indicador  Saúde
#'
#' Constroi os dados de População de todas as idades que está em situação de obesidade
#' de acordo com o Índice de Massa Corporal (IMC) com base no Sisvan/Ministério da Saúde	Saúde
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontru{
#'   data <- constroi_obesid_mun()
#' }
constroi_obesid_mun <- function(){

  url <- "https://docs.google.com/spreadsheets/d/16PPVjMjD00g4UFKR5RDLeaREBOthgoxbv7xyOO_sr4g/edit?gid=1528928475#gid=1528928475"
  obesidade <- purrr::map(
    c(3, 5:8),
    ~ {
      googlesheets4::read_sheet(url,
                         skip = 6, sheet = .x) %>%
        janitor::clean_names() %>%
        dplyr::inner_join(
          target_cities %>%
            dplyr::select(municipio =municipio_nome, uf = estado_sigla) %>%
            dplyr::mutate(municipio = stringi::stri_trans_general(
              municipio, "Latin-ASCII") %>%
                stringr::str_to_upper())
        ) %>%
        dplyr::select(1:5, dplyr::starts_with("obesidade"), total) %>%
        dplyr::mutate(
          dplyr::across(dplyr::starts_with("obesidade"), as.numeric),
          total = as.numeric(total)) %>%
        dplyr::mutate(
          obesidade_total = rowSums(dplyr::select(., dplyr::contains("obesidade")), na.rm = TRUE)
        )
    }
  ) %>%
    purrr::list_rbind() %>%
    dplyr::summarise(
      .by = c(1:5),
      pop_total = sum(total, na.rm = TRUE),
      pop_obesa = sum(obesidade_total, na.rm = TRUE),
      tax_pop_obesa = pop_obesa / pop_total
    ) |>
  dplyr::mutate(
    codigo_ibge = as.character(codigo_ibge)
  ) |>
    dplyr::left_join(
      target_cities |>
        dplyr::mutate(
          codigo_ibge = stringr::str_extract(municipio_codigo, "......")
        ) |>
        dplyr::select(
          codigo_ibge, municipio_codigo
        )
    ) |>
    dplyr::select(-codigo_ibge)

  return(obesidade)
}
