

#' Funcion grafico (esta funcion se usa cuando mi modelo es un objeto "lm")
#'
#' @param y Es el objeto del modelo
#' @param n Son los periodos que se quieren especificar para que grafique lineas verticales de apoyo.
#'
#' @return
#' @export
#'
#' @examples
#'
#' a = c(1,2,3,4,5,6,7,8,9)
#' b = c(2,3,4,5,6,7,8,8,10)
#' al = lm(a~b)
#' plot_function_1(al,5)
#'
#'


plot_function_1 =function(y,n=2){
  k = (y[["residuals"]]+y[["fitted.values"]])
  plot(k,ylim=c(min(y[["residuals"]]),max(k)*1.1),col=2,type = "l",lwd=1)         ## Genera un gráfico con los ejes y nombres
  lines(ts(y[["fitted.values"]]),col=3,lwd=1)   ## Pinta los valores ajustados sobre el gr?fico
  points(ts(y[["residuals"]]),type="l")             ## Pinta los residuales sobre el gr?fico
  abline(h=0,pch=2,lty=2,col="gray50")        ## Pinta la línea de referencia en cero
  abline(v = seq(1, length(y[["residuals"]]), n),col="gray50",pch=2,lty=2)
}

