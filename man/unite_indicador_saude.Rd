% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unite_indicador_saude.R
\name{unite_indicador_saude}
\alias{unite_indicador_saude}
\title{Une as tabelas que compõe o indicador de saúde}
\usage{
unite_indicador_saude(
  cobertura_aps,
  cobertura_ab,
  gastos_per_capta,
  mortalidade_infantil,
  obitos_evitaveis,
  cobertura_vacinal,
  equip_por_estab,
  internacoes,
  obitos,
  subnutricao,
  obesidade
)
}
\arguments{
\item{cobertura_aps}{Tabela com os dados de Cobertura de Assistência Primária}

\item{cobertura_ab}{Tabela com os dados de Cobertura de Assistência Básica}

\item{gastos_per_capta}{Tabela com os dados de Gastor Per Capita em Saúde}

\item{mortalidade_infantil}{Tabela com os dados Mortalidade Infantil para 1000
Nascimentos}

\item{obitos_evitaveis}{Tabela com os dados de Mortalidade por Causas Evitáveis}

\item{cobertura_vacinal}{Tabela com os dados de Cobertura Vacinal}

\item{equip_por_estab}{Tabela com os dados Equipamento Sus por Equipamentos Totais}

\item{internacoes}{Tabela com os dados Internações em Sinistros de Trânsito por
100 mil habitantes}

\item{obitos}{Tabela com os dados Óbitos em Sinistros de Trânsito por
100 mil habitantes}

\item{abastecimento_esgoto}{Tabela com os dados de Acesso a Esgoto e Abastecimento de Águga}
}
\value{
Dataframe com todas as variáveis que compõe o indicador de saúde
}
\description{
Essa função une as tabelas que compões o indicador de saúde em um único dataframe
}
