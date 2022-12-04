#' Adbudg
#'
#' @param X   Es una lista de numeros a la cual se le va a aplicar la transformacion adbook, la funcion tambien admite objetos tipo ts.
#' @param rho Es la funcion que se va a establecer para calcular esta transformacion, por default es la funcion "mean".
#' @param p Es un valor extra que multiplica al valor de rho. El default es igual a 1.
#' @param beta Es un valor extra de la transformacion, este valor esta por default igual a 1
#' @param alpha Es un valor extra de la transformacion, este valor esta por default igual a 0
#' @param v  Es un valor extra de la transformacion, este valor esta por default igual a 1. Si se llegase a
#' colocar un numero mayor a 1 en dicho parametro, la ecuacion de la nueva transformacion seria asi:
#'  \deqn{y = \alpha +{\beta* X^\gamma}/{X^\gamma+\rho^\gamma}}
#'
#' @param begin_year Anho en el cual comienza la serie (este parametro solo se activa si el objeto no es ts)
#' @param begin_period Periodo del Anho en el cual comienza la serie (este parametro solo se activa si el objeto no es ts)
#' @param end_year  Anho en el cual termina la serie (este parametro solo se activa si el objeto no es ts)
#' @param end_period Periodo del Anho en el cual termuina la serie (este parametro solo se activa si el objeto no es ts)
#' @param frecuency Frecuencia del objeto (este parametro solo se activa si el objeto no es ts)
#' @param Ecuation \deqn{y = \alpha +{{\beta*X}/{X+ \rho}} } Se debe tener en cuenta que \eqn{\rho} puede ser el promedio, mediana, valor minimo, etc. De la variable a transformar.
#'
#'
#' @return  Un objeto ts, que es la variable X transformada en adstock
#' @export
#'
#' @examples
#'
#' a = c(1,2,3,34,5,6,7,0,0,0,2,3,4,5)
#' d=ADBUDG(a)
#'
#'
#'
#'
#'
#'

ADBUDG = function(X,v=1,rho=mean,p=1,beta=1,alpha=0,begin_year=start(X)[1],begin_period=start(X)[2],end_year=end(X)[1],end_period=end(X)[2],frecuency=frequency(X)){
  if((is(X)[1]=="ts")){
    if((is(rho)[1]=="numeric")){
      rho = rho*p
      tk=  alpha+((beta*(X^v) )/( (X^v) + (rho^v) ))
      tk <-  ts(tk,frequency = frequency(X), start = start(X),end=end(X))


    } else {
      rho = rho(X)*p
      tk=  alpha+((beta*(X^v) )/(X^v+(rho^v) ))
      tk <- ts(tk,frequency = frequency(X), start = start(X),end=end(X))
    }


  }else {
    if((is(rho)[1]=="numeric")){
      rho = rho*p
      tk=  alpha+((beta*(X^v) )/(X^v+rho^v))

      tk <-  ts(tk,frequency = frecuency, start = c(begin_year,begin_period ),end=c(end_year,end_period ) )
    } else{
      rho = rho(X)*p
      tk=  alpha+((beta*(X^v) )/(X^v+rho^v))

      tk <-  ts(tk,frequency = frecuency, start = c(begin_year,begin_period ),end=c(end_year,end_period ) )
    }

  }
  return(tk)


}







###____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________



#' Logged
#'
#' @param X    Es una lista de numeros a la cual se le va a aplicar la transformacion logged
#' @param beta Es un valor extra de la transformacion, este valor esta por default igual a 1
#' @param alpha Es un valor extra de la transformacion, este valor esta por default igual a 0
#' @param begin_year Anho en el cual comienza la serie (este parametro solo se activa si el objeto no es ts)
#' @param begin_period Periodo del Anho en el cual comienza la serie (este parametro solo se activa si el objeto no es ts)
#' @param end_year  Anho en el cual termina la serie (este parametro solo se activa si el objeto no es ts)
#' @param end_period Periodo del Anho en el cual termuina la serie (este parametro solo se activa si el objeto no es ts)
#' @param frecuency Frecuencia del objeto (este parametro solo se activa si el objeto no es ts)
#' @param Ecuation \deqn{y = \alpha +{\beta*log(X)} }
#'
#'
#' @return la variable transformada.
#' @export
#'
#' @examples
#'
#' a = c(1,2,3,34,5,6,7,8,10,1000,2,3,4,5)
#' Logged(a)
#' b = c(0,2,3,4,5,6,0,1)
#' Logged(b)
#'
#'
#'
#'
#'
#'


Logged = function(X,beta=1,alpha=0,begin_year=start(X)[1],begin_period=start(X)[2],end_year=end(X)[1],end_period=end(X)[2],frecuency=frequency(X)){

 if((is(X)[1]=="ts")){

    tk=    alpha+(beta*log(X))
    tk <-  ts(tk,frequency = frequency(X), start = start(X),end=end(X))

  }else {
    tr=    alpha+(beta*log(X))
    tk <-  ts(tr,frequency = frecuency, start = c(begin_year,begin_period ),end=c(end_year,end_period ) )

  }
    return(tk)
  }




###____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________




#' Power
#'
#' @param X  Es una lista de numeros a la cual se le va a aplicar la transformacion Power.
#' @param beta Es un valor extra de la transformacion, este valor esta por default igual a 1
#' @param v  Es un valor extra de la transformacion, este valor esta por default igual a 1.
#' @param alpha Es un valor extra de la transformacion, este valor esta por default igual a 0
#' @param begin_year Anho en el cual comienza la serie (este parametro solo se activa si el objeto no es ts)
#' @param begin_period Periodo del Anho en el cual comienza la serie (este parametro solo se activa si el objeto no es ts)
#' @param end_year  Anho en el cual termina la serie (este parametro solo se activa si el objeto no es ts)
#' @param end_period Periodo del Anho en el cual termuina la serie (este parametro solo se activa si el objeto no es ts)
#' @param frecuency Frecuencia del objeto (este parametro solo se activa si el objeto no es ts)
#' @param Ecuation  \deqn{y = \alpha +{\beta*(X^\gamma)} }
#'
#'
#' @return la variable transformada, un vector tipo ts.
#' @export
#'
#'
#'
#'
#' @examples
#'
#' a = c(1,2,3,34,5,6,7,8,10,1000,2,3,4,5)
#' Power(a)
#'
#'
#'

Power = function(X,beta=1,v=1,alpha=0,begin_year=start(X)[1],begin_period=start(X)[2],end_year=end(X)[1],end_period=end(X)[2],frecuency=frequency(X)){

  if((is(X)[1]=="ts")){

    tk=  alpha+(beta*(X^v))
    tk <-  ts(tk,frequency = frequency(X), start = start(X),end=end(X))

  }else {
    tr=    alpha+(beta*(X^v))
    tk <-  ts(tr,frequency = frecuency, start = c(begin_year,begin_period ),end=c(end_year,end_period ) )

  }
  return(tk)


}





###____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________



#' Diminishing Exponential
#'
#' @param X  Es una lista de numeros a la cual se le va a aplicar la transformacion Power.
#' @param beta Es un valor extra de la transformacion, este valor esta por default igual a 1
#' @param v Es un valor extra de la transformacion, este valor esta por default igual a 1.
#' @param alpha Es un valor extra de la transformacion, este valor esta por default igual a 0
#' @param begin_year Anho en el cual comienza la serie (este parametro solo se activa si el objeto no es ts)
#' @param begin_period Periodo del Anho en el cual comienza la serie (este parametro solo se activa si el objeto no es ts)
#' @param end_year  Anho en el cual termina la serie (este parametro solo se activa si el objeto no es ts)
#' @param end_period Periodo del Anho en el cual termuina la serie (este parametro solo se activa si el objeto no es ts)
#' @param frecuency Frecuencia del objeto (este parametro solo se activa si el objeto no es ts)
#' @param Ecuation \deqn{y = \alpha +{\beta*{1-exp(-X/\gamma)} }}
#'
#'
#'
#'
#' @return Un vector tipo ts.
#' @export
#'
#' @examples
#'
#'
#' a = c(1,2,3,34,5,6,7,8,10,1000,2,3,4,5)
#' Dim_Exp(a)
#'
#'



Dim_Exp = function(X,beta=1,v=1,alpha=0,begin_year=start(X)[1],begin_period=start(X)[2],end_year=end(X)[1],end_period=end(X)[2],frecuency=frequency(X)){


  if((is(X)[1]=="ts")){

    tk=  alpha+(beta*(1- exp(-X/v) ))
    tk <-  ts(tk,frequency = frequency(X), start = start(X),end=end(X))

  }else {
    tr= alpha+(beta*(1- exp(-X/v) ))
    tk <-  ts(tr,frequency = frecuency, start = c(begin_year,begin_period ),end=c(end_year,end_period ) )

  }
  return(tk)
}



