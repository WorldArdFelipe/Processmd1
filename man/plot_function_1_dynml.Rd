% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_function.R
\name{plot_function_1_dynml}
\alias{plot_function_1_dynml}
\title{Funcion grafico dynlm (esta funcion se usa cuando, la variable respuesta es positiva)}
\usage{
plot_function_1_dynml(EQ1_4,
  begin_year = start((EQ1_4[["residuals"]]))[1],
  begin_month = start((EQ1_4[["residuals"]]))[2],
  end_year = end((EQ1_4[["residuals"]]))[1],
  end_month = end((EQ1_4[["residuals"]]))[2], new_var = rep(0,
  length(EQ1_4[["residuals"]])), p = 1)
}
\arguments{
\item{EQ1_4}{es un modelo hecho con el paquete "dynlm", no funciona si es un modelo hecho con otra funcion.}

\item{begin_year}{Es el a昼㸱o con el cual se quiere comenzar a ver el plot.}

\item{begin_month}{Es el mes en donde comienza el grafico.}

\item{end_year}{Es el a昼㸱o con el cual se quiere terminar de ver el plot.}

\item{end_month}{Es el mes en donde termina el grafico.}

\item{new_var}{es una nueva variable que si se desea se puede incluir en el grafico.}

\item{p}{es el factor de expansion para poder visualizar en escala esta nueva variable.}
}
\value{
un grafico que indica los valores reales de la variable respuesta (linea negra), valores que arroja el modelo (linea roja) y los residuales del modelo (linea azul), nueva variable(linea verde). Adicionalmente el grafico grafica el periodo con el cual fue construido el modelo.
}
\description{
Funcion grafico dynlm (esta funcion se usa cuando, la variable respuesta es positiva)
}
\examples{


library(dynlm)

A<-rnorm(10)
B<-rnorm(10)
C<-rnorm(10)
y<- c(1,1.1,1.3,1.5,1.6,1.7,1.2,1.3,1.2,1.2)
data=cbind(y,A,B,C)
model<-dynlm(y~A+B+C+L(y,1),data=data)
plot_function_1_dynml(model)




}
