#' Obter Empregos Formais por 100k Habitantes
#'
#' Esta função retorna a quantidade de empregos formais por 100.000 habitantes para os municípios,
#' com base nos dados de empregos formais de 2022 e a população dos municípios.
#'
#' Ela realiza o seguinte processo:
#' \itemize{
#'   \item Carrega os dados de empregos formais de 2022 usando a função `get_empregos_formais_total_2022()`.
#'   \item Carrega os dados de população dos municípios e realiza as transformações necessárias, como a criação da coluna `municipio_codigo`, remoção das colunas `cod_uf` e `cod_munic`, e renomeação de colunas.
#'   \item Realiza o `left_join` entre os dados de empregos formais e a população, e calcula a quantidade de empregos formais por 100.000 habitantes.
#' }
#'
#' @return Um data frame contendo os dados de empregos formais por 100.000 habitantes para os municípios.
#'         As colunas retornadas incluem:
#'         \itemize{
#'           \item `municipio_codigo`: Código único do município.
#'           \item `municipio_nome`: Nome do município.
#'           \item `estado_sigla`: Sigla do estado.
#'           \item `populacao`: População do município.
#'           \item `empregos_por_100khab`: Empregos formais por 100.000 habitantes no município.
#'         }
#'
#' @examples
#' # Exemplo de uso:
#' empregos_por_100khab <- get_empregos_formais_por_100khab()
#' head(empregos_por_100khab)
#'
#' @export
get_empregos_formais_por_100khab <- function() {
  ano <- 2022
  nome_coluna_empregos <- paste0('total_empregos_formais_', ano)

  total_empregos_2022 <- get_empregos_formais_total(download = TRUE, ano = ano) %>%
    select(municipio_codigo, total_empregos = !!nome_coluna_empregos)

  populacao <- reviewAgroCData::pop_municipios %>%
    mutate(municipio_codigo = paste0(cod_uf, cod_munic)) %>%
    select(-cod_uf, -cod_munic) %>%
    rename(estado_sigla = uf,
           municipio_nome = nome_do_municipio) %>%
    select(municipio_codigo, municipio_nome, estado_sigla, populacao)

  empregos_por_100khab <- left_join(total_empregos_2022, populacao, by = "municipio_codigo") %>%
    mutate(empregos_por_100khab = (total_empregos / populacao) * 100000)

  return(empregos_por_100khab)
}
