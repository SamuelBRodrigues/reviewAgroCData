#' Extração dados de Óbitos por Causas Evitáveis DATASUS
#'
#' A função extrai os dados óbitos por Causas evitáveis em menores do que 5 anos e
#' óbitos por Causas evitáveis entre 5 a 75 anos direto da plataforma do DATASUS
#' através do pacote datasus a nível municipal.
#'
#' @param ano Ano de interesse dos dados
#'
#' @returns Um dataframe com os dados óbitos evitáveis e as causas evitáveis a nível municipal
#' @export
#'
#' @examples
#' \dontrun{
#'   get_obitos_evitaveis(ano = "2023")
#' }
get_obitos_evitaveis <- function(ano = 2023){

  # Pegando os dados de morte evitaveis para menor do que 5 anos
  mortes_evitaveis_menor_5 <- datasus::sim_evita10_mun(coluna = "Causas evitáveis",periodo = ano)

  # Pegando os dados de morte evitaveis para maior do que 5 anos
  mortes_evitaveis_maior_5 <- datasus::sim_evitb10_mun(coluna = "Causas evitáveis",periodo = ano)

  # Estruturando os dados
  data <- mortes_evitaveis_menor_5 |>
    # Renomeando o valor total de acordo com cada base
    dplyr::rename(
      "Total_menor_5" = Total
    ) |>
    dplyr::left_join(
      mortes_evitaveis_maior_5 |>
        dplyr::rename(
          "Total_maior_5" = Total
        ),
      by = join_by(Município),
      suffix = c("_menor_5", "_maior_5")
    ) |>
    # Transformando em um tibble
    tibble::tibble() |>
    # Retirando as linhas "MUNICIPIO IGNORADO"
    subset(
      !stringr::str_detect(Município, "MUNICIPIO IGNORADO")
    ) |>
    # Removendo a soma total
    tail(-1) |>
    # Pegando o codigo das UF e o nome dos municípios
    dplyr::mutate(
      cod_uf = stringr::str_extract(Município, ".."),
      Município = stringr::str_extract(Município, '\\D+$') |> stringr::str_remove(" ")
    ) |>
    # Renomeando a coluna município
    dplyr::rename(
      "nome_do_municipio" = Município
    ) |>
    # Adicionando os códigos do ibge dos municípios
    dplyr::left_join(
      pop_municipios |>
        # Criando a coluna com os códigos do ibge dos municípios
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = "",
          remove = FALSE
        ) |>
        # Corrigindo nomes dos município
        dplyr::mutate(
          nome_do_municipio = stringr::str_to_upper(abjutils::rm_accent(nome_do_municipio)),
          municipio_codigo = as.character(municipio_codigo),
          cod_uf = as.character(cod_uf)
        ) |>
        # Selecionando as colunas de interesse
        dplyr::select(
          nome_do_municipio, municipio_codigo, cod_uf
        ),
    ) |>
    # Tratando os NA
    dplyr::mutate(
      dplyr::across(-c(nome_do_municipio, municipio_codigo),
                    ~ tidyr::replace_na(.x, 0))
    ) |>
    # Removendo colunas que não são de interesse
    dplyr::select(
      -nome_do_municipio, -cod_uf
    ) |>
    dplyr::mutate(
      ano = ano
    ) |>
    dplyr::relocate(
      municipio_codigo,
      ano
    ) |>
    janitor::clean_names()

}
