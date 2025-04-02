#' Extração dos dados de Internações por acidentes de trânsito por munícipio do DATASUS
#'
#' A função extrai os dados de internações por acidente de trânsito a nível municipal
#' por ano da plataforma do DATASUS. A função retorna uma tabela com nome do município,
#' quantidade de internações, código do estado segundo IBGE e o ano dos dados
#'
#' @param ano Ano dos dados que serão requisitados. É necessário colocar apenas a década
#' do ano de interesse no formato string, por exemplo: 2022, ano = "22"
#'
#' @returns Uma tabela com nome do município, quantidade de internações, código
#' do estado segundo IBGE e o ano dos dados
#' @export
#'
#' @examples
#' \dontrun{
#'   get_internacoes_DATASUS(ano = "09")
#' }
get_internacoes_DATASUS <- function(ano){

  # Construindo o body requisitado
  body <- stringr::str_glue("Linha=Munic%EDpio&Coluna=--N%E3o-Ativa--&Incremento=Interna%E7%F5es&Arquivos=fibr{ano}12.dbf&Arquivos=fibr{ano}11.dbf&Arquivos=fibr{ano}10.dbf&Arquivos=fibr{ano}09.dbf&Arquivos=fibr{ano}08.dbf&Arquivos=fibr{ano}07.dbf&Arquivos=fibr{ano}06.dbf&Arquivos=fibr{ano}05.dbf&Arquivos=fibr{ano}04.dbf&Arquivos=fibr{ano}03.dbf&Arquivos=fibr{ano}02.dbf&Arquivos=fibr{ano}01.dbf&pesqmes1=Digite+o+texto+e+ache+f%E1cil&SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&SCapital=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde_%28CIR%29=TODAS_AS_CATEGORIAS__&pesqmes4=Digite+o+texto+e+ache+f%E1cil&SMacrorregi%E3o_de_Sa%FAde=TODAS_AS_CATEGORIAS__&pesqmes5=Digite+o+texto+e+ache+f%E1cil&SMicrorregi%E3o_IBGE=TODAS_AS_CATEGORIAS__&pesqmes6=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_Metropolitana_-_RIDE=TODAS_AS_CATEGORIAS__&pesqmes7=Digite+o+texto+e+ache+f%E1cil&STerrit%F3rio_da_Cidadania=TODAS_AS_CATEGORIAS__&pesqmes8=Digite+o+texto+e+ache+f%E1cil&SMesorregi%E3o_PNDR=TODAS_AS_CATEGORIAS__&SAmaz%F4nia_Legal=TODAS_AS_CATEGORIAS__&SSemi%E1rido=TODAS_AS_CATEGORIAS__&SFaixa_de_Fronteira=TODAS_AS_CATEGORIAS__&SZona_de_Fronteira=TODAS_AS_CATEGORIAS__&SMunic%EDpio_de_extrema_pobreza=TODAS_AS_CATEGORIAS__&SCar%E1ter_atendimento=TODAS_AS_CATEGORIAS__&SRegime=TODAS_AS_CATEGORIAS__&pesqmes16=Digite+o+texto+e+ache+f%E1cil&SGrande_Grup_Causas=1&pesqmes17=Digite+o+texto+e+ache+f%E1cil&SGrupo_de_Causas=TODAS_AS_CATEGORIAS__&pesqmes18=Digite+o+texto+e+ache+f%E1cil&SCategorias_Causas=TODAS_AS_CATEGORIAS__&pesqmes19=Digite+o+texto+e+ache+f%E1cil&SFaixa_Et%E1ria_1=TODAS_AS_CATEGORIAS__&pesqmes20=Digite+o+texto+e+ache+f%E1cil&SFaixa_Et%E1ria_2=TODAS_AS_CATEGORIAS__&SSexo=TODAS_AS_CATEGORIAS__&SCor%2Fra%E7a=TODAS_AS_CATEGORIAS__&zeradas=exibirlz&formato=table&mostre=Mostra")

  # Link da plataforma do DATASUS
  url <- "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sih/cnv/fibr.def"

  # Requisição para acesso aos dados de internações por acidente de transporte
  req <- httr2::request(url) |>
    httr2::req_method("POST") |> # Utilizando o método POST para enviar dados para API
    httr2::req_headers(
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
      "Accept-Language" = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
      "Cache-Control" = "max-age=0",
      "Connection" = "keep-alive",
      "Content-Type" = "application/x-www-form-urlencoded",
      "Cookie" = "TS014879da=01e046ca4cff6a91fa35cb6e25205f3594991a508ff08da628f24378fe54c9ed5e0418dbb9bdb313829312c6e2daa6a8b03ba5ec34",
      "Origin" = "http://tabnet.datasus.gov.br",
      "Referer" = "http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sih/cnv/fibr.def",
      "Upgrade-Insecure-Requests" = "1",
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36 OPR/117.0.0.0"
    ) |>
    httr2::req_body_raw(body)

  # Performando a requisição
  resp <- httr2::req_perform(req)

  # Extraindo os dados de interesse
  internacoes <- resp |> # A requisão retorna um html
    httr2::resp_body_html() |>
    # Dentro html lemos a tabela
    rvest::html_table() |>
    # A tabela de interesse é a tabela de index 1
    purrr::pluck(1) |>
    # Estruturação dos dados
    janitor::row_to_names(2) |>
    janitor::clean_names() |>
    tail(-2) |>
    dplyr::select(municipio, internacoes) |>
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
    subset(
      !stringr::str_detect(municipio, 'Municipio Ignorado')
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
        dplyr::select(municipio, uf, municipio_codigo, populacao),
      by = join_by(municipio, uf)
    ) |>
    dplyr::mutate(
      taxa_internacoes_100k_hab = (internacoes/populacao)*100000
    )

  return(internacoes)
}
