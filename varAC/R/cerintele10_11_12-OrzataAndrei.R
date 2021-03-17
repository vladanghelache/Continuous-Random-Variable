#Orzata Andrei, 232

#Cerinta 10:

#densitatea comuna a celor doua variabile
#f <- function(x,y)
#{
#  if(0 < x && x<y)
#    return ((0.64)*(1/exp((0.8)*y)))
#  else
#    return(0)
#}


#' covVAC
#'
#' Functia covVAC() preia in f densitatea comuna a variabilelor continue aleatoare X si Y, calculand valorile asteptate / mediile lui X, Y si a combinatiei dintre cele doua  si intoarce conform formulei de mai sus, covariatia.
#' @param f densitatea comuna a variabilelor continue aleatoare X si Y
#' @keywords VA
#' @export
#' @examples
#' covVAC(f)
covVAC <- function(f)
{

  #Valoarea a??teptat?/Media fiecaror variabile in parte

  EXY <- integrate(function(y) { sapply(y, function(y) {
    integrate(function(x) x*y*f(x,y), 0, y)$value
  })
  }, 0, Inf)$value

  EX <- integrate(function(y) { sapply(y, function(y) {
    integrate(function(x) x*f(x,y), 0, y)$value
  })
  }, 0, Inf)$value

  EY <- integrate(function(z) { sapply(y, function(y) {
    integrate(function(x) y*f(x,y), 0, Inf)$value
  })
  }, 0, Inf)$value


  return (EXY-(EX*EY))
}

#' cvVAC
#'
#' Functia cvVAC() preia in f densitatea comuna a variabilelor continue aleatoare X si Y, calculeaza deviatia fiecaruia si o stocheaza in variabilele devX, devY si conform formulei de mai sus, utilizand functia de covariatie covVAC(), returneaza valoare coeficientului de corelatie.
#' @param f densitatea comuna a variabilelor continue aleatoare X si Y
#' @keywords VA
#' @export
#' @examples
#' cvVAC(f)
cvVAC <- function(f)
{
  #Calculam deviatia standard pentru X si Y
  devX <- integrate(function(x) x*f(x,y), 0, y)$value
  devY <- integrate(function(x) y*f(x,y), 0, Inf)$value

  return (covVAC()/(devX*devY))
}

#Cerinta 11:

#Densitati Marginale

#' dMarX
#'
#' In functiile dMarY() si dMarX() se preia in f densitatea comuna a variabilelor continue aleatoare X si Y si conform formulei descrise anterior se returneza valorile densitatii marginala in variabila val.
#' @param f densitatea comuna a variabilelor continue aleatoare X si Y
#' @keywords VA
#' @export
#' @examples
#' dMarX(f)
dMarX <- function(f)
{

  val <- integrate(function(x) f(x,y), 0, y)$value

  return (val)
}

#' dMarY
#'
#' In functiile dMarY() si dMarX() se preia in f densitatea comuna a variabilelor continue aleatoare X si Y si conform formulei descrise anterior se returneza valorile densitatii marginala in variabila val.
#' @param f densitatea comuna a variabilelor continue aleatoare X si Y
#' @keywords VA
#' @export
#' @examples
#' dMarY(f)
dMarY <- function(f)
{

  val <- integrate(function(y) f(x,y), 0, Inf)$value

  return (val)
}

#Densitati Conditionale

#' dCondYX
#'
#' In dCondXY(), dCondYX(), folosind functiile mentionate anterior si media/ valoarea asteptata a densitatii comune a variabilelor continue aleatoare X si Y, se calculeaza conform formulei si se returneaza valorile densitatii cond in val
#' @param f densitatea comuna a variabilelor continue aleatoare X si Y
#' @keywords VA
#' @export
#' @examples
#' dCondYX(f)
dCondYX <- function(f)
{
  fxy <- integrate(function(y) { sapply(y, function(y) {
    integrate(function(x) x*y*f(x,y), 0, y)$value
  })
  }, 0, Inf)$value
  val <- (fxy/dMarX())

  return (val)

}

#' dCondXY
#'
#' In dCondXY(), dCondYX(), folosind functiile mentionate anterior si media/ valoarea asteptata a densitatii comune a variabilelor continue aleatoare X si Y, se calculeaza conform formulei ai se returneaza valorile densitatii cond in val
#' @param f densitatea comuna a variabilelor continue aleatoare X si Y
#' @keywords VA
#' @export
#' @examples
#' dCondXY(f)
dCondXY <- function(f)
{
  fxy <- integrate(function(y) { sapply(y, function(y) {
    integrate(function(x) x*y*f(x,y), 0, y)$value
  })
  }, 0, Inf)$value
  val <- (fxy/dMarX())

  return (val)
}


#Cerinta 12:

#funcX <- function(a)
#{
#  return(a)
#}

#funcY <- function(b)
#{
#  return(b)
#}

#' SumDif
#'
#' Functia SumDif() preia densitatile a doua variabile aleatoare continue X si Y, in funcX() si funcY(), si  folosind formulele matematice anterioare calculeaza densitatea sumei acestora in funcZT(), precum variatia si media diferentelor sumei si diferente, urmand ca acestea sa fie salvate in variabile miux, miuy si varx, vary respectiv, urmand ca acestea sa fie calculate pentru suma si diferenta si afiaate in consola cu ajutorul functiei print().
#' @param funcX densitatea unei variabile continue aleatoare x
#' @param funcY  densitatea unei variabile continue aleatoare Y
#' @keywords VA
#' @export
#' @examples
#' SumDif(f)
SumDif <- function(funcX, funcY)
{


  funcZT <- function(t)
  {
    val <- integrate(function(x) funcX(x)*funcY(t-x), -Inf, Inf)$value
    return(val)

  }

  miuX <- integrate(funcX, -Inf, Inf)$value
  miuY <- integrate(funcX, -Inf, Inf)$value
  varX <- integrate(function(x) (x^2)*funcX(X),-Inf,Inf)$value - miux^2
  varY <-integrate(function(y)  y^2*funcy(y),-Inf,Inf)$value  - miuy^2

  print("SUMA")
  print(miux+miuY)
  print(varx+varY)

  print("DIFERENTA")
  print(miux-miuY)
  print(varx+varY)


}
