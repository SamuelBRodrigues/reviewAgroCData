#' Extrai os dados de Equipamento por Estabelecimento do Tabnet DATASUS
#'
#' A função extrai os dados de Equipamentos por Estabelecimento e Número de Equipamentos
#' Existentes de cada município para um determinado ano e mês. Os dados são extraídos
#' diretamente do CNES - EQUIPOBR da plataforma Tabnet DATASUS
#'
#' @param ano Ano de interesse dos dados. Por padrão usa os dados mais recentes.
#' @param mes Mes de interesse dos dados. Por padrão usa os dados mais recentes.
#'
#' @returns Um dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   equip_por_estab_mun <- get_equip_estab(ano = "23", mes = "04")
#' }
get_equip_estab <- function(ano = "25", mes = "02"){

  # URL da base
  url <- "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?cnes/cnv/equipobr.def"

  # Body da requisição para API
  body_raw <- stringr::str_glue("Linha=Munic%EDpio&Coluna=--N%E3o-Ativa--&Incremento=Equipamentos_Existentes&Incremento=Estab_c%2F_Equip_SUS&Arquivos=eqbr{ano}{mes}.dbf&SRegi%E3o=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&SUnidade_da_Federa%E7%E3o=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes4=Digite+o+texto+e+ache+f%E1cil&SMunic%EDpio_gestor=TODAS_AS_CATEGORIAS__&pesqmes5=Digite+o+texto+e+ache+f%E1cil&SCapital=TODAS_AS_CATEGORIAS__&pesqmes6=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde_%28CIR%29=TODAS_AS_CATEGORIAS__&pesqmes7=Digite+o+texto+e+ache+f%E1cil&SMacrorregi%E3o_de_Sa%FAde=TODAS_AS_CATEGORIAS__&pesqmes8=Digite+o+texto+e+ache+f%E1cil&SMicrorregi%E3o_IBGE=TODAS_AS_CATEGORIAS__&pesqmes9=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_Metropolitana_-_RIDE=TODAS_AS_CATEGORIAS__&pesqmes10=Digite+o+texto+e+ache+f%E1cil&STerrit%F3rio_da_Cidadania=TODAS_AS_CATEGORIAS__&pesqmes11=Digite+o+texto+e+ache+f%E1cil&SMesorregi%E3o_PNDR=TODAS_AS_CATEGORIAS__&SAmaz%F4nia_Legal=TODAS_AS_CATEGORIAS__&SSemi%E1rido=TODAS_AS_CATEGORIAS__&SFaixa_de_Fronteira=TODAS_AS_CATEGORIAS__&SZona_de_Fronteira=TODAS_AS_CATEGORIAS__&SMunic%EDpio_de_extrema_pobreza=TODAS_AS_CATEGORIAS__&SEnsino%2FPesquisa=TODAS_AS_CATEGORIAS__&pesqmes18=Digite+o+texto+e+ache+f%E1cil&SNatureza_Jur%EDdica=TODAS_AS_CATEGORIAS__&pesqmes19=Digite+o+texto+e+ache+f%E1cil&SEsfera_Jur%EDdica=TODAS_AS_CATEGORIAS__&SEsfera_Administrativa=TODAS_AS_CATEGORIAS__&pesqmes21=Digite+o+texto+e+ache+f%E1cil&SNatureza=TODAS_AS_CATEGORIAS__&pesqmes22=Digite+o+texto+e+ache+f%E1cil&STipo_de_Estabelecimento=TODAS_AS_CATEGORIAS__&STipo_de_Gest%E3o=TODAS_AS_CATEGORIAS__&STipo_de_Prestador=TODAS_AS_CATEGORIAS__&pesqmes25=Digite+o+texto+e+ache+f%E1cil&SEquipamento=TODAS_AS_CATEGORIAS__&SGrupo_de_Equipamentos=TODAS_AS_CATEGORIAS__&SEquipamento_selecionado=TODAS_AS_CATEGORIAS__&zeradas=exibirlz&formato=table&mostre=Mostra", "application/x-www-form-urlencoded")

  # Construindo a requisição
  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_body_raw(body_raw)

  # Fazendo a requisição
  resp <- httr2::req_perform(req)

  # Extraindo os dados de interesse
  csv_file <- resp |> # A requisão retorna um html
    httr2::resp_body_html() |>
    rvest::html_node("body") |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset(".csv$+")

  # Construindo o link do arquivo csv
  link_csv_file <- stringr::str_glue("http://tabnet.datasus.gov.br{csv_file}")

  # Lendo o arquivo csv
  data <- read_delim(link_csv_file, delim = ";", locale = locale(encoding = "latin1"),
                     skip = 3) %>%
    head(-3) %>%
    subset(
      !stringr::str_detect(.$Município, "MUNICIPIO IGNORADO")
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      uf = stringr::str_extract(municipio, '\\d{2}'),
      municipio = stringr::str_extract(municipio, '[^\\d{6}[:blank:]{1}].+') |> stringr::str_to_title()
    ) |>
    dplyr::mutate(
      dplyr::across(-municipio,
                    ~as.numeric(.x))
    ) |>
    # Aplicando tratamento dos NA
    dplyr::mutate(
      dplyr::across(-municipio,
                    ~tidyr::replace_na(.x, 0)),
      year = stringr::str_glue("20{ano}")
    ) |>
    dplyr::left_join(
      pop_municipios |>
        dplyr::mutate(
          municipio = stringr::str_to_title(abjutils::rm_accent(nome_do_municipio)),
          uf = cod_uf,
        ) |>
        tidyr::unite(
          "municipio_codigo",
          cod_uf:cod_munic,
          sep = "",
          remove = F
        ) |>
        dplyr::select(municipio, uf, municipio_codigo),
      by = join_by(municipio, uf)
    ) |>
    dplyr::mutate(
      perc_equip_sus_nos_estab_saude = (estab_c_equip_sus/equipamentos_existentes) * 100
    ) |>
    dplyr::relocate(
      municipio, municipio_codigo, uf, year
    )

  return(data)

}
