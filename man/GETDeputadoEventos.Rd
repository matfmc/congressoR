% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{GETDeputadoEventos}
\alias{GETDeputadoEventos}
\title{Função que retorna Uma lista de eventos com a participação do parlamentar}
\usage{
GETDeputadoEventos(
  id,
  dataInicio = NULL,
  dataFim = NULL,
  pagina = NULL,
  itens = NULL,
  ordem = NULL,
  ordenarPor = NULL
)
}
\arguments{
\item{id}{Identificador numérico do parlamentar.}

\item{dataInicio}{Data de início de um intervalo de tempo, no formato AAAA-MM-DD.}

\item{dataFim}{Data de término de um intervalo de tempo, no formato AAAA-MM-DD.}

\item{pagina}{Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.}

\item{itens}{Número máximo de itens na página que se deseja obter com esta requisição.}

\item{ordem}{O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.}

\item{ordenarPor}{Qual dos elementos da representação deverá ser usado para aplicar ordenação à lista.}
}
\description{
Retorna uma lista de objetos evento nos quais a participação do parlamentar identificado por {id} era ou é prevista.
Um período de tempo pode ser delimitado para a busca.
Se não forem passados parâmetros de tempo, são retornados os eventos num período de cinco dias, sendo dois antes e dois depois do dia da requisição.
Os itens podem ser ordenados por id, siglaOrgao ou dataHoraInicio.
}
\examples{
GETDeputadoDiscurso(id = '204554')
}
\keyword{deputados}
