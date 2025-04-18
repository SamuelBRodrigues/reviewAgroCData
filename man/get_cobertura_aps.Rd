% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cobertura_aps.R
\name{get_cobertura_aps}
\alias{get_cobertura_aps}
\title{Extração dos dados de Cobertura de Atenção Primária a Saúde (APS)}
\usage{
get_cobertura_aps(ano = 2024)
}
\arguments{
\item{ano}{Ano de interesse dos dados. Por padrão usa o ano de 2024}
}
\value{
Um dataframe
}
\description{
A função extrai os dados de Cobertura APS diretamente da plataforma e-Gestor
do ministério da Saúde. Os dados são detalhados a nível municipal para o ano
e mês inserido
}
\examples{
\dontrun{
  dados_cobertura_aps <- get_cobertura_aps(
    ano = "2024",
    mes = "01"
  )
}
}
