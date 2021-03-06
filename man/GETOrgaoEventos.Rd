% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{GETOrgaoEventos}
\alias{GETOrgaoEventos}
\title{Função que retorna Os eventos ocorridos ou previstos em um órgão legislativo}
\usage{
GETOrgaoEventos(
  id,
  idTipoEvento = NULL,
  dataInicio = NULL,
  dataFim = NULL,
  pagina = NULL,
  itens = NULL,
  ordem = NULL,
  ordenarPor = NULL
)
}
\arguments{
\item{id}{Identificador numérico do órgão}

\item{idTipoEvento}{Um ou mais identificador(es) numérico(s) de tipos de evento que se deseja obter. os valores válidos podem ser obtidos por uma requisição anterior a /referencias/tiposEvento.}

\item{dataInicio}{Data de início de um intervalo de tempo, no formato AAAA-MM-DD.}

\item{dataFim}{Data de término de um intervalo de tempo, no formato AAAA-MM-DD.}

\item{pagina}{Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.}

\item{itens}{Número máximo de itens na página que se deseja obter com esta requisição.}

\item{ordem}{O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.}

\item{ordenarPor}{Qual dos elementos da representação deverá ser usado para aplicar ordenação à lista.}
}
\description{
Retorna uma lista de informações resumidas dos eventos realizados (ou a realizar) pelo órgão legislativo identificado por {id}.
Por padrão, são retornados eventos em andamento ou previstos para o mesmo dia, dois dias antes e dois dias depois da requisição. Parâmetros podem ser passados para alterar esse período, bem como os tipos de eventos.
}
\examples{
GETOrgaoEventos()
}
\keyword{orgãos}
