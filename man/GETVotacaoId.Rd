% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{GETVotacaoId}
\alias{GETVotacaoId}
\title{Função que retorna Informações detalhadas sobre uma votação da Câmara}
\usage{
GETVotacaoId(id)
}
\arguments{
\item{id}{Identificador alfanumérico da votação}
}
\description{
Retorna um conjunto detalhado de dados sobre a votação identificada por {id}, tais como as proposições que podem ter sido o objeto da votação e os efeitos de tramitação de outras proposições que eventualmente tenham sido cadastrados em consequência desta votação.
Para compreender melhor os dados retornados, veja o tutorial sobre votações do Portal de Dados Abertos.
}
\examples{
GETVotacaoId()
}
\keyword{votações}
