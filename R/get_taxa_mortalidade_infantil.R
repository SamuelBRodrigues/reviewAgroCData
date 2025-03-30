#' Extração dados de Mortalidade Infantil e Nascimento DATASUS
#'
#' A função extrai os dados de mortalidade infantil e nascimento direto da plataforma
#' do DATASUS através do pacote datasus e calcula a taxa de mortalidade infantil para
#' 1000 nascimentos a nível municipal.
#'
#' @param ano Ano de interesse dos dados
#'
#' @returns Um dataframe com os dados de Mortalidade Infantil, Nascimento, Taxa
#' de Mortaliade Infantil para 1000 nascimentos a nível municipal
#' @export
#'
#' @examples
#' \dontrun{
#'   get_taxa_mortalidade_infantil(ano = "2023")
#' }
get_taxa_mortalidade_infantil <- function(ano = "2022"){

  data_mortalidade <- datasus::sim_inf10_mun(periodo = ano) |>
    tibble::tibble() |>
    tail(-1) |>
    dplyr::mutate(
      cod_uf = stringr::str_extract(Município, ".."),
      Município = stringr::str_extract(Município, '\\D+$') |> stringr::str_remove(" ")
    ) |>
    subset(
      !stringr::str_detect(Município, 'IGNORADO')
    ) |>
    dplyr::left_join(
      pop_municipios |>
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = "",
          remove = FALSE
        ) |>
        dplyr::mutate(
          nome_do_municipio = stringr::str_to_upper(abjutils::rm_accent(nome_do_municipio)),
          municipio_codigo = as.character(municipio_codigo),
          cod_uf = as.character(cod_uf)
        ) |>
        dplyr::select(
          nome_do_municipio, municipio_codigo, cod_uf
        ),
      by = join_by(Município == nome_do_municipio, cod_uf == cod_uf)
    ) |>
    dplyr::rename(
      "mortalidade_infantil" = `Óbitos p/Residênc`
    ) |>
    dplyr::select(
      municipio_codigo, mortalidade_infantil, cod_uf
    )

  data_nascimento <- datasus::sinasc_nv_mun(periodo = ano) |>
    tibble::tibble() |>
    tail(-1) |>
    dplyr::mutate(
      cod_uf = stringr::str_extract(Município, ".."),
      Município = stringr::str_extract(Município, '\\D+$') |> stringr::str_remove(" ")
    ) |>
    subset(
      !stringr::str_detect(Município, 'IGNORADO')
    ) |>
    dplyr::left_join(
      pop_municipios |>
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = "",
          remove = FALSE
        ) |>
        dplyr::mutate(
          nome_do_municipio = stringr::str_to_upper(abjutils::rm_accent(nome_do_municipio)),
          municipio_codigo = as.character(municipio_codigo),
          cod_uf = as.character(cod_uf)
        ) |>
        dplyr::select(
          nome_do_municipio, municipio_codigo, cod_uf
        ),
      by = join_by(Município == nome_do_municipio, cod_uf == cod_uf)
    ) |>
    dplyr::rename(
      "nascimentos" = `Nascim p/resid.mãe`
    ) |>
    dplyr::select(
      municipio_codigo, nascimentos, cod_uf
    )

  data <- data_nascimento |>
    dplyr::left_join(
      data_mortalidade,
      by = join_by(municipio_codigo, cod_uf)
    ) |>
    dplyr::mutate(
      nascimentos = tidyr::replace_na(nascimentos, 0),
      mortalidade_infantil = tidyr::replace_na(mortalidade_infantil, 0),
      taxa_mortalidade_infantil = mortalidade_infantil/nascimentos * 1000
    )

  return(data)

}
