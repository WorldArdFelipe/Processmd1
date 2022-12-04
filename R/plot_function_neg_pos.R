#' Funcion grafico 2 dynlm (esta funcion se usa cuando, la variable respuesta tiene valores negativos y positivos)
#'
#' @param EQ1_4 es un modelo hecho con el paquete "dynlm", no funciona si es un modelo hecho con otra funcion.
#' @param begin_year Es el año con el cual se quiere comenzar a ver el plot.
#' @param begin_month Es el mes en donde comienza el grafico.
#' @param end_year Es el año con el cual se quiere terminar de ver el plot.
#' @param end_month Es el mes en donde termina el grafico.
#' @param new_var es una nueva variable que si se desea se puede incluir en el grafico.
#' @param p es el factor de expansion para poder visualizar en escala esta nueva variable.
#' @return un grafico que indica los valores reales de la variable respuesta (linea negra), valores que arroja el modelo (linea roja) y los residuales del modelo (linea azul)
#' @export
#'
#' @examples
#'
#'
#' library(dynlm)
#' y<-arima.sim(model=list(ar=c(.9)),n=10)
#' A<-rnorm(10)
#' B<-rnorm(10)
#' C<-rnorm(10)
#' y<-y+.5*A+.2*B-.3*C
#' data=cbind(y,A,B,C)
#' model<-dynlm(y~A+B+C+L(y,1),data=data)
#' plot_function_2_dynml(model)
#'
#'
#'


plot_function_2_dynml=function(EQ1_4,begin_year=start((EQ1_4[["residuals"]]))[1],begin_month=start((EQ1_4[["residuals"]]))[2],end_year=end((EQ1_4[["residuals"]]))[1],end_month=end((EQ1_4[["residuals"]]))[2],new_var= rep(0,length(EQ1_4[["residuals"]])),p =1){
  ETK = EQ1_4[["residuals"]]
  if(is(ETK)[1]=="ts"){

    qt = ts(new_var*p,start=start(EQ1_4[["residuals"]]),end=end(EQ1_4[["residuals"]]),frequency=frequency(EQ1_4[["residuals"]]))

    model = EQ1_4

    k = (model[["residuals"]]+model[["fitted.values"]])
    plot(k,ylim=c(1.5*min(k),max(k)),col="black",type = "l",lwd=1)
    lines((model[["fitted.values"]]),col="red",lwd=1)   ## Pinta los valores ajustados sobre el gr?fico
    points((model[["residuals"]]),type="l",col="darkblue")
    points(qt,type="l",col="green")
    abline(h=0,pch=2,lty=2,col="gray50")        ## Pinta la lÃ???nea de referencia en cero
    abline(v=c(seq(start(model[["residuals"]])[1],end(model[["residuals"]])[1]+(end(model[["residuals"]])[2]/12),1/model[["frequency"]][1])),pch=2,lty=2,col="gray50")


  } else{
    qt = ts(new_var*p,start=start(EQ1_4[["residuals"]]),end=end(EQ1_4[["residuals"]]),frequency=frequency(EQ1_4[["residuals"]]))
    k = (EQ1_4[["residuals"]]+EQ1_4[["fitted.values"]])
    plot(k,ylim=c(min(k),max(k)*1.1),col="black",type = "l",lwd=1)
    lines(ts(EQ1_4[["fitted.values"]]),col="red",lwd=1)   ## Pinta los valores ajustados sobre el gr?fico
    points(ts(EQ1_4[["residuals"]]),type="l",col="darkblue")
    points(qt,type="l",col="green")
    abline(h=0,pch=2,lty=2,col="gray50")        ## Pinta la lÃ?nea de referencia en cero
    abline(v=c(seq(start(EQ1_4[["residuals"]]),end(EQ1_4[["residuals"]]),1/EQ1_4[["frequency"]][1])),pch=2,lty=2,col="gray50")


  }


}







