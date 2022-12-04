
#' Adstock
#'
#' @param X     Es un vector de numeros el cual se le va a aplicar la transformacion adstock, la funcion tambien admite objetos tipo ts.
#' @param rate  Es la tasa con la cual se va a calcular el adstock, esta funciona con el numero entero es decir si por ejemplo se quiere calcular un adstock de 50%, se coloca 50.
#' @param frecuency         Frecuencia del objeto.
#' @param begin_year     Anho en el cual comienza la serie.
#' @param begin_period     Periodo del Anho en el cual comienza la serie.
#' @param end_year    Anho en el cual termina la serie.
#' @param end_period      Periodo del Anho en el cual termina la serie.
#'
#' @return  Un objeto ts, que es la variable X transformada en adstock.
#' @export
#'
#' @examples
#'
#'
#' a=c(1,2,3,4,5,56,6,7,4,2,3)
#' ADSTOCK(a,50)
#'
#'
#'

ADSTOCK  = function(X,rate=0,begin_year=start(X)[1],begin_period=start(X)[2],end_year=end(X)[1],end_period=end(X)[2],frecuency=frequency(X)){
  if((is(X)[1]=="ts")){
    pri = numeric(length(X))
    pri[1] = X[1]
    for(i in 2:length(X)){
      pri[i] = X[i] + (rate/100 )* pri[i-1]
    }

    a=tsp(pri)

    #aa = c(floor(a[1]),floor(a[2]),floor(a[3]),a[1]%%1,a[2]%%1)

    pri = ts(pri,frequency = frequency(X), start = start(X),end=end(X))
  } else{

    adstocked_advertising = numeric(length(X))
    adstocked_advertising[1] = X[1]
    for(i in 2:length(X)){
      adstocked_advertising[i] = X[i] + (rate/100) * adstocked_advertising[i-1]
    }
    #return(adstocked_advertising)

    pri <-  ts((adstocked_advertising),frequency = frecuency, start = c(begin_year, begin_period),end=c(end_year,end_period))
  }
  return(pri)
}
