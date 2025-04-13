#' Controi os dados de Subnutrição
#'
#' Essa função constroi os dados de Subnutrição do Sisvan/Ministério da Saúde	 População de todas as
#' faixas etárias que estão abaixo do peso ideal, a saber:
#' crianças de 0 – 10 anos (peso muito baixo, peso baixo para a idade),
#' adolescentes (magreza acentuada para a idade),
#' adultos, idosos e gestantes (baixo peso).
#'
#' @param diretorio_sisvan Diretório onde se encontra os dados baixados do Sisvan.
#'
#' @returns Uma dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- constroi_subnutricao_sisvan(
#'     diretorio_sisvan = "data_raw/sisvan.xlsx"
#'   )
#' }
constroi_subnutricao_sisvan <- function(diretorio_sisvan = "./data_raw/sisvan.xlsx"){

  crianca_0_10 <- purrr::map(
    1:2,
    ~{
      peso_colunas <- readxl::read_excel(diretorio_sisvan,
                                         skip = 6) %>%
        janitor::clean_names() %>%
        names()

      readxl::read_excel(diretorio_sisvan,
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
        dplyr::select(1:6, 8) %>%
        dplyr::mutate(
          dplyr::across(dplyr::starts_with("peso"), as.numeric),
          dplyr::across(dplyr::starts_with("codigo"), as.numeric))
    }
  ) %>%
    purrr::list_rbind() %>%
    dplyr::summarise(
      .by = c(1:5),
      crianca_0_10 = sum(peso_muito_baixo_para_a_idade, peso_baixo_para_a_idade,
                         na.rm = TRUE)
    )
  # Adultos idoso e gestantes
  adultos_idosos_gestantes <- purrr::map(
    3:5,
    ~{
      peso_colunas <- readxl::read_excel(diretorio_sisvan,
                                         skip = 6, sheet = 3) %>%
        janitor::clean_names() %>%
        names()

      readxl::read_excel(diretorio_sisvan,
                         skip = 6,
                         #col_names = peso_colunas,
                         sheet = .x) %>%
        janitor::clean_names() %>%
        dplyr::select(1:6) %>%
        dplyr::mutate(baixo_peso = as.numeric(baixo_peso)) %>%
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
      adultos_idoso_gestantes = sum(baixo_peso, na.rm = TRUE)
    )


  adolescentes <-  readxl::read_excel(diretorio_sisvan,
                                      skip = 6, sheet = 6) %>%
    janitor::clean_names() %>%
    dplyr::inner_join(
      target_cities %>%
        dplyr::select(municipio =municipio_nome, uf = estado_sigla) %>%
        dplyr::mutate(municipio = stringi::stri_trans_general(
          municipio, "Latin-ASCII") %>%
            stringr::str_to_upper())
    ) %>%
    dplyr::mutate(adolescentes = as.numeric(magreza_acentuada)) %>%
    dplyr::select(1:5, adolescentes)

  # unindo tudo
  subnutricao <-
    crianca_0_10 %>%
    dplyr::left_join(adultos_idosos_gestantes) %>%
    dplyr::left_join(adolescentes)

  return(subnutricao)
}
