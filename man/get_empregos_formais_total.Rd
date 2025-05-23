% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_empregos_formais_total.R
\name{get_empregos_formais_total}
\alias{get_empregos_formais_total}
\title{Obter Dados de Empregos Formais Totais por Ano}
\usage{
get_empregos_formais_total(download = TRUE, ano = 2022)
}
\arguments{
\item{download}{Lógico indicando se deve baixar os dados caso não existam localmente
(padrão: TRUE)}

\item{ano}{Ano dos dados a serem obtidos (padrão: 2022). Atualmente suporta
os anos de 2020 a 2024.}
}
\value{
Um data frame contendo:
\describe{
  \item{municipio_codigo}{Código IBGE do município}
  \item{municipio_nome}{Nome do município}
  \item{estado_sigla}{Sigla da UF do município}
  \item{total_empregos_formais_[ano]}{Número total de empregos formais no ano especificado}
}
}
\description{
Esta função processa os dados da RAIS (Relação Anual de Informações Sociais) para retornar
o número total de empregos formais para cidades-alvo específicas em um determinado ano.
Ela faz o download (opcional) e lê os dados da RAIS, filtra-os com base nas cidades-alvo
predefinidas, e retorna as informações relevantes.
}
\examples{
# Obter total de empregos formais para as cidades-alvo em 2022
total_empregos <- get_empregos_formais_total(ano = 2022)
head(total_empregos) # A coluna será chamada total_empregos_formais_2022
}
