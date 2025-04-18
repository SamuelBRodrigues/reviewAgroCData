% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pega_dados_indicadores.R
\name{pega_dados_indicadores}
\alias{pega_dados_indicadores}
\title{Pega os dados de todos os indicadores}
\usage{
pega_dados_indicadores(
  cod_ibge = target_cities$municipio_codigo,
  ano = "last",
  mes = "03",
  download = TRUE,
  dir = "data_raw"
)
}
\arguments{
\item{cod_ibge}{Vetor com códigos IBGE. Padrão: target_cities$municipio_codigo}

\item{ano}{Ano de referência. Padrão: Mais recente ("last")}

\item{mes}{Mês de referência. Padrão: Mais recente ("last")}

\item{download}{Lógico. Se TRUE, baixa os dados. Padrão: TRUE}

\item{dir}{Diretório dos dados CRAS. Padrão: "data_raw"}
}
\value{
Um dataframe
}
\description{
Essa função é responsável por pegar os dados de todos os indicadores e unir todos
em uma única tabela
}
\examples{
\dontrun{
  data <- pega_dados_indicadores(
    cod_ibge = target_cities$municipio_codigo, ano = "last", mes = "06", download = TRUE,
    dir = "data_raw"
  )
}
}
