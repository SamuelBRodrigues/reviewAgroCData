#' Extração o número de familias e pessoas em situação de pobreza do Cadúnico
#'
#' Essa função extrai o número de pessoas e famílias em situação de pobreza e que
#' estão cadastradas no CadÚnico da plataforma Aplicações Cidadania.
#'
#' @param cod_municipios_ibge Lista com o código dos munícipios do IBGE. Como padrão
#' utiliza a coluna municipios codigo da tabela target_cities.
#'
#' @returns Um data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'  get_situacao_pobreza_data(cod_municipios_ibge = c("1508126", "1708205", "2101400"))
#' }
get_situacao_pobreza_data <- function(cod_municipios_ibge = target_cities$municipio_codigo){

  cores <- future::availableCores()

  future::plan(future::multisession(), workers = cores)

  data <- furrr::future_map(
    cod_municipios_ibge,
    ~{
      # Pegando os 6 primeiros digites do código do municipio
      cod_ibge <- stringr::str_extract(.x, "^.*(?=.$)")

      # Construindo o url que contém os dados do município
      url <- stringr::str_glue("https://cecad.cidadania.gov.br/painel03.php?&oik=1&p_ibge={cod_ibge}")

      # Lendo o html do site
      html <- rvest::read_html(url)

      # Pegano o mês do dado consultado
      mes <- html |>
        rvest::html_element(
          css = "#dados-cadastro-dois"
        ) |>
        rvest::html_element(
          css = "p"
        ) |>
        rvest::html_text2() |>
        stringr::str_extract("\\d+")
      # Pegando o ano do dado consultado
      ano <- html |>
        rvest::html_element(
          css = "#dados-cadastro-dois"
        ) |>
        rvest::html_element(
          css = "p"
        ) |>
        rvest::html_text2() |>
        stringr::str_extract("\\d+$")

      # Pegando o valor de familia e pessoas cadastradas no CadÚnico em situação de pobreza
      situacao_pobreza <- html |>
        rvest::html_elements(
          css = "#dados-cadastro-dois > div:nth-child(2) > div > div:nth-child(1) > div"
        ) |>
        rvest::html_elements("h5") |>
        rvest::html_text() |>
        # Removendo a pontuação
        stringr::str_replace('\\.', "") |>
        stringr::str_extract('\\d+')

      # Famílias em situação de pobreza
      familias_situacao_pobreza <- situacao_pobreza[1]

      # Pessoas em situação de pobreza
      pessoas_situacao_pobreza <- situacao_pobreza[2]

      # Construindo uma tabela com os dados
      data <- tibble::tibble(
        cod_ibge = .x,
        ano = ano,
        mes = mes,
        familias_situacao_pobreza = familias_situacao_pobreza |> as.numeric(),
        pessoas_situacao_pobreza = pessoas_situacao_pobreza |> as.numeric()
      ) |>
        dplyr::left_join(
          pop_municipios |>
            tidyr::unite(
              "cod_ibge",
              cod_uf:cod_munic,
              sep = ""
            ) |>
            dplyr::select(
              cod_ibge, nome_do_municipio
            )
        ) |>
        dplyr::relocate(
          cod_ibge, nome_do_municipio, ano, mes
        )

    },
    .progress = TRUE,
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = list(
        pop_municipios = reviewAgroCData::pop_municipios
      )
    )
  )

}
