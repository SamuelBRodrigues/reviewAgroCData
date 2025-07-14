#' Pega os dados de Paridade de Gênero e Paridade de Negros das Câmaras Municipais -
#' Indicador de Proteção Social
#'
#' Essa função pega os dados de Paridade de Gênero e Paridade de Negros de Câmaras
#' Municipais diretamente do TSE
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- get_paridades_camara_mun()
#' }
get_paridades_camara_mun <- function(){
  dados_eleicao <- electionsBR::elections_tse(2024, type = "candidate") %>%
    dplyr::filter(DS_CARGO== "VEREADOR",
                  DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")
    ) %>%
    dplyr::select(
      SG_UF, SG_UE, NM_UE,
      DS_COR_RACA,DS_GENERO) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      sg_uf = dplyr::case_match(
        sg_uf,
        "AC" ~ "12",
        "AL" ~ "27",
        "AP" ~ "16",
        "AM" ~ "13",
        "BA" ~ "29",
        "CE" ~ "23",
        "DF" ~ "53",
        "ES" ~ "32",
        "GO" ~ "52",
        "MA" ~ "21",
        "MT" ~ "51",
        "MS" ~ "50",
        "MG" ~ "31",
        "PA" ~ "15",
        "PB" ~ "25",
        "PR" ~ "41",
        "PE" ~ "26",
        "PI" ~ "22",
        "RJ" ~ "33",
        "RN" ~ "24",
        "RS" ~ "43",
        "RO" ~ "11",
        "RR" ~ "14",
        "SC" ~ "42",
        "SP" ~ "35",
        "SE" ~ "28",
        "TO" ~ "17"
      )
    ) |>
    dplyr::left_join(
      pop_municipios |>
        tidyr::unite(
          "cod_ibge",
          cod_uf:cod_munic,
          sep = "",
          remove = FALSE
        ) |>
        dplyr::select(
          sg_uf = cod_uf, nm_ue = nome_do_municipio, cod_ibge
        ) |>
        dplyr::mutate(
          nm_ue = stringr::str_to_upper(nm_ue),
          sg_uf = as.character(sg_uf)
        )
    )

  # paridade de genero
  par_genero_mun <- dados_eleicao %>%
    dplyr::group_by(sg_uf, nm_ue, cod_ibge) %>%
    dplyr::count(ds_genero) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::filter(ds_genero == "FEMININO") %>%
    dplyr::summarise(
      total = unique(total),
      percentual_paridade_genero = n / total * 100,
      .groups = "drop"
    )


  # paridade de genero e cor

  par_neg_mun <- dados_eleicao %>%
    dplyr::group_by(sg_uf, nm_ue, cod_ibge) %>%
    dplyr::count(ds_cor_raca) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::filter(ds_cor_raca %in% c("PARDA", "PRETA")) %>%
    dplyr::summarise(
      total = unique(total),
      negros = sum(n),
      percentual_paridade_negros = negros / total * 100,
      .groups = "drop"
    )

  data <- par_neg_mun %>%
    dplyr::full_join(par_genero_mun) %>%
    dplyr::select(-negros)

  return(data)
}
