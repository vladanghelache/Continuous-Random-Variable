
#Anghelache Vlad-Alexandru, 232

#Cerinta 1: constanta de normalizare
#sursa teorie: https://en.wikipedia.org/wiki/Normalizing_constant

# 'verifica' daca functia este pozitiva pentru valori intre -10^35 si 10^35 + calculeaza limitele la inf si -inf si verifiva daca acestea sunt negative
verif <- function(f){
  min <- optimise(f, interval = c(-10000000000000000000000000000000000,10000000000000000000000000000000000))$objective
  l1 <- f(-Inf)
  l2 <- f(Inf)
  if(min < 0 || l1 < 0 || l2 < 0)
  {
    return(0)
  }
  return(1)
}






#' getNConst
#'
#' The getNConst function returns a normalizing constant for a positive function. If the function doesn't have one, it returns NULL and prints the following message ‘Nu exista constanta de normalizare pentru functia data!’. If an error occurs, the message for that specific error will also be printed.
#' @param f a function
#' @keywords VA
#' @export
#' @examples
#' getNConst(f)
getNConst <- function(f){
  tryCatch(
  {
    ok <- verif(f)
    if(ok == 0)
    {
      print("Functia nu este pozitiva!")
      return()
    }
    val <- integrate(f,-Inf,Inf)$value

    if(val <= 0)
    {
      print("Nu exista constanta de normalizare pentru functia data!")
      return()
    }
    else{
      return(val^(-1))
    }
  },
  error=function(cond){
    print("Nu exista constanta de normalizare pentru functia data!")
    print(cond)
    return()
  }
  )

}


#Teste:
#test <- function(x)
#{
#  exp(-x^2/2)
#}

#{getNConst(test)}

#test2 <- function(x)
#{
#  exp(-x^2/2)*getNConst(test)
#}

#{getNConst(test2)}

#test3 <- function(x)
#{
# x
#}


#{getNConst(test3)}




#Cerinta 2: Verificare daca o functie este densitate de probabilitate

#' isDensity
#'
#' This function  check if a function is a probability density function.
#' @param f a function.
#' @keywords VA
#' @export
#' @examples
#' isDensity(f)
isDensity <- function(f)
{
  tryCatch(
    {
      if(verif(f)==0) #se verifica daca functia este pozitiva cu ajutorul functiei verif
      {
        return(0)
      }
      else {
        i <- integrate(f,-Inf,Inf)$value
        if(i != 1) #se verifica daca integrala sa de la -inf la inf este 1
        {
          return(0)
        }
      }
      return(1) # se returneaza 1 daca functia f este densitate de probabilitate si 0 in caz contrar
    },
    error=function(cond){ #in cazul unei erori, se afiseaza mesajul eroarii, iar 'isDensity' returneaza 0
      print(cond)
      return(0)
    }
  )

}



#{isDensity(test3)}
#{isDensity(test2)}
#{isDensity(test)}








#Cerinta 3: Creaarea unui obiect dee tip variabila aleatoare continua

makeRep <- function(f) #functia primeste densitatea de probabilitate si returneaza repartitia
{
  return(function(x)
    {
      integrate(f,-Inf, x)$value
    })

}


setClass("contRV",representation(d = "function", r = "function"))

validRV <- function(object) #se verifica daca functia data este densitate de probabilitate
{
  if(isDensity(object@d) == 1)
  {
    TRUE
  }
  else{
    paste("Functia nu este densitate de probabilitate")
  }
}
setValidity("contRV", validRV)

#se construieste repartitia r la initializare
setMethod("initialize", "contRV",
          function(.Object, ...){
            .Object <- callNextMethod()

            .Object@r <- makeRep(.Object@d)
            .Object
          })

#' makeVA
#'
#' The function makeVA  makes a continuous random variable object which contains  the cumulative distribution function r and the probability density function d.
#' @param f a probability density function.
#' @keywords VA
#' @export
#' @examples
#' makeVA(f)
makeVA <- function(f){

  v1 <- new("contRV", d = f)

  return(v1)

}

#v1 <- makeVA(test2)


#v1@r(Inf)













