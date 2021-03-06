% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{GETPartidoMembros}
\alias{GETPartidoMembros}
\title{Função que retorna Uma lista dos parlamentares de um partido durante um período}
\usage{
GETPartidoMembros(
  id,
  dataInicio = NULL,
  dataFim = NULL,
  idLegislatura = NULL,
  pagina = NULL,
  itens = NULL,
  ordem = NULL,
  ordenarPor = NULL
)
}
\arguments{
\item{id}{Id do Partido}

\item{dataInicio}{Data de início de um intervalo de tempo, no formato AAAA-MM-DD.}

\item{dataFim}{Data de término de um intervalo de tempo, no formato AAAA-MM-DD.}

\item{idLegislatura}{Número da(s) legislatura(s), separados por vírgulas, às quais os dados buscados devem corresponder.}

\item{pagina}{Qual dos elementos da representação deverá ser usado para aplicar ordenação à lista.}

\item{itens}{O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.}

\item{ordem}{Número máximo de itens na “página” que se deseja obter com esta requisição.}

\item{ordenarPor}{Número da “página” de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.}
}
\description{
Retorna uma lista de deputados que estão ou estiveram em exercício pelo partido {id}.
Opcionalmente, pode-se usar os parâmetros dataInicio, dataFim ou idLegislatura para se obter uma lista de deputados filiados ao partido num certo intervalo de tempo. Isso é equivalente ao serviço /deputados com filtro por partido, mas é melhor para obter informações sobre membros de partidos já extintos.
}
\examples{
GETPartidoId()
}
\keyword{partidos}
