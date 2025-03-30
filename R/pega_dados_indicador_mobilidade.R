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
    req_method("POST") |> # Utilizando o método POST para enviar dados para API
    req_headers(
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
    req_body_raw(body)

  # Performando a requisição
  resp <- httr2::req_perform(req)

  # Extraindo os dados de interesse
  internacoes <- resp |> # A requisão retorna um html
    resp_body_html() |>
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
        dplyr::select(municipio,uf, populacao)
    ) |>
    dplyr::mutate(
      taxa_internacoes_100k_hab = (internacoes/populacao)*100000
    )

  return(internacoes)
}

#' Extração dos dados de Óbitos por acidentes de trânsito por munícipio do DATASUS
#'
#' A função extrai os dados de óbitos por acidente de trânsito a nível municipal
#' por ano da plataforma do DATASUS. A função retorna uma tabela com nome do município,
#' quantidade de óbitos, código do estado segundo IBGE e o ano dos dados
#'
#' @param ano Ano dos dados que serão requisitados. É necessário colocar apenas a década
#' do ano de interesse no formato string, por exemplo: 2008, ano = "08"
#'
#' @returns Uma tabela com nome do município, quantidade de óbitos, código
#' do estado segundo IBGE e o ano dos dados
#' @export
#'
#' @examples
#' \dontrun{
#'   get_obitos_DATASUS(ano = "09")
#' }
get_obitos_DATASUS <- function(ano){

  body <- stringr::str_glue("Linha=Munic%EDpio&Coluna=--N%E3o-Ativa--&Incremento=%D3bitos&Arquivos=fibr{ano}12.dbf&Arquivos=fibr{ano}11.dbf&Arquivos=fibr{ano}10.dbf&Arquivos=fibr{ano}09.dbf&Arquivos=fibr{ano}08.dbf&Arquivos=fibr{ano}07.dbf&Arquivos=fibr{ano}06.dbf&Arquivos=fibr{ano}05.dbf&Arquivos=fibr{ano}04.dbf&Arquivos=fibr{ano}03.dbf&Arquivos=fibr{ano}02.dbf&Arquivos=fibr{ano}01.dbf&pesqmes1=Digite+o+texto+e+ache+f%E1cil&SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&SCapital=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde_%28CIR%29=TODAS_AS_CATEGORIAS__&pesqmes4=Digite+o+texto+e+ache+f%E1cil&SMacrorregi%E3o_de_Sa%FAde=TODAS_AS_CATEGORIAS__&pesqmes5=Digite+o+texto+e+ache+f%E1cil&SMicrorregi%E3o_IBGE=TODAS_AS_CATEGORIAS__&pesqmes6=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_Metropolitana_-_RIDE=TODAS_AS_CATEGORIAS__&pesqmes7=Digite+o+texto+e+ache+f%E1cil&STerrit%F3rio_da_Cidadania=TODAS_AS_CATEGORIAS__&pesqmes8=Digite+o+texto+e+ache+f%E1cil&SMesorregi%E3o_PNDR=TODAS_AS_CATEGORIAS__&SAmaz%F4nia_Legal=TODAS_AS_CATEGORIAS__&SSemi%E1rido=TODAS_AS_CATEGORIAS__&SFaixa_de_Fronteira=TODAS_AS_CATEGORIAS__&SZona_de_Fronteira=TODAS_AS_CATEGORIAS__&SMunic%EDpio_de_extrema_pobreza=TODAS_AS_CATEGORIAS__&SCar%E1ter_atendimento=TODAS_AS_CATEGORIAS__&SRegime=TODAS_AS_CATEGORIAS__&pesqmes16=Digite+o+texto+e+ache+f%E1cil&SGrande_Grup_Causas=1&pesqmes17=Digite+o+texto+e+ache+f%E1cil&SGrupo_de_Causas=TODAS_AS_CATEGORIAS__&pesqmes18=Digite+o+texto+e+ache+f%E1cil&SCategorias_Causas=TODAS_AS_CATEGORIAS__&pesqmes19=Digite+o+texto+e+ache+f%E1cil&SFaixa_Et%E1ria_1=TODAS_AS_CATEGORIAS__&pesqmes20=Digite+o+texto+e+ache+f%E1cil&SFaixa_Et%E1ria_2=TODAS_AS_CATEGORIAS__&SSexo=TODAS_AS_CATEGORIAS__&SCor%2Fra%E7a=TODAS_AS_CATEGORIAS__&zeradas=exibirlz&formato=table&mostre=Mostra")

  url <- "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sih/cnv/fibr.def"

  # Requisição para acesso aos dados de internações por acidente de transporte
  req <- request(url) |>
    req_method("POST") |> # Utilizando o método POST para enviar dados para API
    req_headers(
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
    req_body_raw("Linha=Munic%EDpio&Coluna=--N%E3o-Ativa--&Incremento=%D3bitos&Arquivos=fibr2212.dbf&Arquivos=fibr2211.dbf&Arquivos=fibr2210.dbf&Arquivos=fibr2209.dbf&Arquivos=fibr2208.dbf&Arquivos=fibr2207.dbf&Arquivos=fibr2206.dbf&Arquivos=fibr2205.dbf&Arquivos=fibr2204.dbf&Arquivos=fibr2203.dbf&Arquivos=fibr2202.dbf&Arquivos=fibr2201.dbf&pesqmes1=Digite+o+texto+e+ache+f%E1cil&SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&SCapital=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde_%28CIR%29=TODAS_AS_CATEGORIAS__&pesqmes4=Digite+o+texto+e+ache+f%E1cil&SMacrorregi%E3o_de_Sa%FAde=TODAS_AS_CATEGORIAS__&pesqmes5=Digite+o+texto+e+ache+f%E1cil&SMicrorregi%E3o_IBGE=TODAS_AS_CATEGORIAS__&pesqmes6=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_Metropolitana_-_RIDE=TODAS_AS_CATEGORIAS__&pesqmes7=Digite+o+texto+e+ache+f%E1cil&STerrit%F3rio_da_Cidadania=TODAS_AS_CATEGORIAS__&pesqmes8=Digite+o+texto+e+ache+f%E1cil&SMesorregi%E3o_PNDR=TODAS_AS_CATEGORIAS__&SAmaz%F4nia_Legal=TODAS_AS_CATEGORIAS__&SSemi%E1rido=TODAS_AS_CATEGORIAS__&SFaixa_de_Fronteira=TODAS_AS_CATEGORIAS__&SZona_de_Fronteira=TODAS_AS_CATEGORIAS__&SMunic%EDpio_de_extrema_pobreza=TODAS_AS_CATEGORIAS__&SCar%E1ter_atendimento=TODAS_AS_CATEGORIAS__&SRegime=TODAS_AS_CATEGORIAS__&pesqmes16=Digite+o+texto+e+ache+f%E1cil&SGrande_Grup_Causas=1&pesqmes17=Digite+o+texto+e+ache+f%E1cil&SGrupo_de_Causas=TODAS_AS_CATEGORIAS__&pesqmes18=Digite+o+texto+e+ache+f%E1cil&SCategorias_Causas=TODAS_AS_CATEGORIAS__&pesqmes19=Digite+o+texto+e+ache+f%E1cil&SFaixa_Et%E1ria_1=TODAS_AS_CATEGORIAS__&pesqmes20=Digite+o+texto+e+ache+f%E1cil&SFaixa_Et%E1ria_2=TODAS_AS_CATEGORIAS__&SSexo=TODAS_AS_CATEGORIAS__&SCor%2Fra%E7a=TODAS_AS_CATEGORIAS__&zeradas=exibirlz&formato=table&mostre=Mostra")

  # Performando a requisição
  resp <- httr2::req_perform(req)

  # Extraindo os dados de interesse
  obitos <- resp |>
    # A requisão retorna um html
    resp_body_html() |>
    # Dentro html lemos a tabela
    rvest::html_table() |>
    # A tabela de interesse é a tabela de index 1
    purrr::pluck(1) |>
    # Estruturação dos dados
    janitor::row_to_names(2) |>
    janitor::clean_names() |>
    tail(-2) |>
    dplyr::select(municipio, obitos) |>
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
        dplyr::select(municipio, uf, populacao)
    ) |>
    dplyr::mutate(
      taxa_obito_100k_hab = (obitos/populacao)*100000
    )

  return(obitos)


}

get_deslocamento_SIDRA <- function(){

  deslocamento <- sidrar::get_sidra(api = '/t/3422/n6/all/v/1001319/p/all/c490/12829,12830/d/v1001319%202') |>
    janitor::clean_names() |>
    dplyr::select(
      municipio_codigo, municipio, variavel, ano, tempo_habitual_de_deslocamento_para_o_trabalho, valor
    ) |>
    tidyr::pivot_wider(
      names_from = tempo_habitual_de_deslocamento_para_o_trabalho,
      values_from = valor
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      "Percentual da população que gasta 1hora ou mais no deslocamento casatrabalho (total e por faixa de renda)" =
        mais_de_uma_hora_ate_duas_horas + mais_de_duas_horas
    )
  return(deslocamento)

}

#' Extrai os dados de Emissão por População
#'
#' A função extra os dados de Emissão por População do SEEG Municípios.
#'
#' @param dir Diretório onde o arquivo será salvo
#'
#' @returns Retorna uma tabela com os dados de Emissão por Populção a nível municipal
#' @export
#'
#' @examples
#' \dontrun{
#'   get_emissao(dir = "data_raw")
#' }
get_emissao <- function(dir = "data_raw"){

  # Checa se o arquivo POP2022_Municipios existe
  if(!file.exists(stringr::str_glue("{dir}/emissao_pop_munic_clean.xlsx"))){
    dir.create(stringr::str_glue("{dir}/"))
    download.file("https://public.tableau.com/vizql/w/SEEGMUNICPIOS2022Estatsticas/v/RANKINGGERAL/tempfile/sessions/FDEE1C89E0E540D9A42E16C69D3201E0-0:0/?key=798408527&keepfile=yes&attachment=yes",
                  destfile = stringr::str_glue("{dir}/emissao_pop_munic.xlsx"),
                  mode = "wb")
    emissao <- readxl::read_xlsx(stringr::str_glue("{dir}/emissao_pop_munic.xlsx"), skip = 1) |>
      dplyr::mutate(
        dplyr::across(-c(`CÓD. IBGE`,Município, UF),
                      ~ tidyr::replace_na(.x, 0)
        )
      ) |>
      dplyr::mutate(
        emissao_por_pop = AC + AL + AM + AP + BA + CE + DF + ES + GO + MA + MG + MS +
          MT + PA + PB + PE + PI + PR + RJ + RN + RO + RR + RS + SC + SE + SP + TO
      ) |>
      janitor::clean_names() |>
      dplyr::select(
        cod_ibge, municipio, uf, emissao_por_pop
      )

    writexl::write_xlsx(emissao, stringr::str_glue('{dir}/emissao_pop_munic_clean.xlsx'))

    return(emissao)

  } else{
    emissao <- readxl::read_xlsx(stringr::str_glue('{dir}/emissao_pop_munic_clean.xlsx'))
  }
  return(emissao)

}
