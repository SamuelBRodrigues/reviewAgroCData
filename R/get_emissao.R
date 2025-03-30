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
