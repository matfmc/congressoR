% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{GETProposicaoAutores}
\alias{GETProposicaoAutores}
\title{Função que retorna Lista pessoas e/ou entidades autoras de uma proposição}
\usage{
GETProposicaoAutores(id)
}
\arguments{
\item{id}{Identificador numérico da proposição}
}
\description{
Retorna uma lista em que cada item identifica uma pessoa ou entidade que é autora da proposição identificada por {id}. Além de deputados, também podem ser autores de proposições os senadores, a sociedade civil, assembleias legislativas e os poderes Executivo e Judiciário.
Pelo Regimento da Câmara, todos os que assinam uma proposição são considerados autores (art. 102), tanto os proponentes quanto os apoiadores.
Para obter mais informações sobre cada autor, é recomendável acessar, se disponível, a URL que é valor do campo uri.
}
\examples{
GETProposicaoAutores()
}
\keyword{proposições}
