% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_perc_pastagens_boas_cond.R
\name{get_perc_pastagens_boas_cond}
\alias{get_perc_pastagens_boas_cond}
\title{Extrai os dados de Estabelecimentos Agropecuários com Pastagens em Boas condições}
\usage{
get_perc_pastagens_boas_cond(cod_ibge = "Brasil")
}
\arguments{
\item{cod_ibge}{Um vetor de caracteres com o código IBGE do município ou "Brasil" para todos os municípios ou "Targets" para os municípios alvo.}
}
\value{
Um dataframe
}
\description{
A função extrai  dados de Estabelecimentos Agropecuários com Pastagens em Boas
condições a nivel municipal do Senso Agro de 2017 através da API do SIDRA IBGE.
}
\examples{
\dontrun{
  data <- get_perc_pastagens_boas_cond()
}
}
