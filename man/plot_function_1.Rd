% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_function_2.R
\name{plot_function_1}
\alias{plot_function_1}
\title{Funcion grafico (esta funcion se usa cuando mi modelo es un objeto "lm")}
\usage{
plot_function_1(y, n = 2)
}
\arguments{
\item{y}{Es el objeto del modelo}

\item{n}{Son los periodos que se quieren especificar para que grafique lineas verticales de apoyo.}
}
\description{
Funcion grafico (esta funcion se usa cuando mi modelo es un objeto "lm")
}
\examples{

a = c(1,2,3,4,5,6,7,8,9)
b = c(2,3,4,5,6,7,8,8,10)
al = lm(a~b)
plot_function_1(al,5)


}
