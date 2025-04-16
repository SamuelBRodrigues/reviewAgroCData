#' Controi os dados de Subnutrição - Indicador de Saúde
#'
#' Essa função constroi os dados de Subnutrição do Sisvan/Ministério da Saúde	 População de todas as
#' faixas etárias que estão abaixo do peso ideal, a saber:
#' crianças de 0 – 10 anos (peso muito baixo, peso baixo para a idade),
#' adolescentes (magreza acentuada para a idade),
#' adultos, idosos e gestantes (baixo peso).
#'
#' @returns Uma dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- constroi_subnutricao_sisvan()
#' }
constroi_subnutricao_sisvan <- function(){

  url <- "https://docs.google.com/spreadsheets/d/16PPVjMjD00g4UFKR5RDLeaREBOthgoxbv7xyOO_sr4g/edit?gid=1528928475#gid=1528928475"
  crianca_0_10 <- purrr::map(
    1:2,
    ~{
      peso_colunas <- googlesheets4::read_sheet(url,
                                                skip = 6) %>%
        janitor::clean_names() %>%
        names()

      googlesheets4::read_sheet(url,
                                skip = 6,
                                col_names = peso_colunas, sheet = .x) %>%
        janitor::clean_names() %>%
        dplyr::inner_join(
          target_cities %>%
            dplyr::select(municipio =municipio_nome, uf = estado_sigla) %>%
            dplyr::mutate(municipio = stringi::stri_trans_general(
              municipio, "Latin-ASCII") %>%
                stringr::str_to_upper())
        ) %>%
        dplyr::select(1:6, 8, total) %>%
        dplyr::mutate(
          dplyr::across(dplyr::starts_with("peso"), as.numeric),
          dplyr::across(dplyr::starts_with("codigo"), as.numeric),
          total = as.numeric(total))
    }
  ) %>%
    purrr::list_rbind() %>%
    dplyr::summarise(
      .by = c(1:5),
      total_pop_crianca_0_10 = sum(total, na.rm = TRUE),
      crianca_0_10 = sum(peso_muito_baixo_para_a_idade, peso_baixo_para_a_idade,
                         na.rm = TRUE),
      taxa_crianca_0_10 = crianca_0_10 / total_pop_crianca_0_10
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("codigo"), as.character)
    )
  # Adultos idoso e gestantes
  adultos_idosos_gestantes <- purrr::map(
    3:5,
    ~{
      peso_colunas <- googlesheets4::read_sheet(url,
                                                skip = 6, sheet = .x) %>%
        janitor::clean_names() %>%
        names()

      googlesheets4::read_sheet(url,
                                #col_names = peso_colunas,
                                sheet = 5,
                                range = "A:N",
                                col_types = c("ccccci???????i"))|>
        tail(-5) |>
        janitor::row_to_names(1) |>
        janitor::clean_names() |>
        dplyr::select(1:6,14)|>
        dplyr::rename(
          baixo_peso = 6,
          total = 7
        ) %>%
        dplyr::inner_join(
          target_cities %>%
            dplyr::select(municipio =municipio_nome, uf = estado_sigla) %>%
            dplyr::mutate(municipio = stringi::stri_trans_general(
              municipio, "Latin-ASCII") %>%
                stringr::str_to_upper())
        )
    }
  ) %>%
    purrr::list_rbind() %>%
    dplyr::summarise(
      .by = c(1:5),
      total_pop_adultos_idoso_gestantes = sum(total, na.rm = TRUE),
      adultos_idoso_gestantes = sum(baixo_peso, na.rm = TRUE),
      taxa_adultos_idoso_gestantes = adultos_idoso_gestantes / total_pop_adultos_idoso_gestantes
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("codigo"), as.character)
    )


  adolescentes <-  googlesheets4::read_sheet(url,
                                             skip = 6, sheet = 6) %>%
    janitor::clean_names() %>%
    dplyr::inner_join(
      target_cities %>%
        dplyr::select(municipio =municipio_nome, uf = estado_sigla) %>%
        dplyr::mutate(municipio = stringi::stri_trans_general(
          municipio, "Latin-ASCII") %>%
            stringr::str_to_upper())
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("codigo"), as.character),
                  adolescentes = as.numeric(magreza_acentuada),
                  taxa_adolescentes = adolescentes/total) %>%
    dplyr::select(1:5, adolescentes, total_adolescentes_pop = total, taxa_adolescentes)

  # unindo tudo
  subnutricao <-
    crianca_0_10 |>
    dplyr::left_join(adultos_idosos_gestantes) %>%
    dplyr::left_join(adolescentes) |>
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

  return(subnutricao)
}
