#' Pega os dados de Paridade de Gênero e Paridade de Negros das Câmaras Municipais
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
    dplyr::inner_join(
      target_cities %>%
        dplyr::select(nm_ue =municipio_nome, sg_uf = estado_sigla,
                      cod_muni = municipio_codigo) %>%
        dplyr::mutate(nm_ue = stringr::str_to_upper(nm_ue),
                      sg_uf = stringr::str_to_upper(sg_uf))
    )

  # paridade de genero
  par_genero_mun <- dados_eleicao %>%
    dplyr::group_by(sg_uf, nm_ue, cod_muni) %>%
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
    dplyr::group_by(sg_uf, nm_ue, cod_muni) %>%
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
