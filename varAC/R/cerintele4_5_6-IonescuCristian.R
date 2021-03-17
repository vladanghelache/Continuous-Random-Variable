#Ionescu Cristian-Andrei

#ex4

#' printRandomDp
#'
#' This function generate some prints on density and distribution function for multiple random repartitions.
#' @keywords VA
#' @export
#' @examples
#' printRandomDp()
printRandomDp <- function() {
  x <- seq(0,100,by = 1)
  y <- dbinom(x,50,0.5)
  f <- pbinom(55, 100, 0.01)

  plot(x,y)
  plot(55, f)

  scores <- seq(-3, 3, by = .1)
  scores
  dValues <- dnorm(scores)
  dValues

  plot(dvalues)
}

#' printContRV
#'
#' The printContRV function generate the graphic representations of the probability density function and the cumulative distribution function for a given continuous RV
#' @param RV a continuous random variable
#' @param i start of sequence
#' @param j end of sequence
#' @keywords VA
#' @export
#' @examples
#' printContRV(RV, i, j)
printContRV <- function(RV, i, j){
  x <- seq(i,j,by = 0.005)
  plot(x, RV@d(x), main = "probability density function")
  aux <-function(f,x)
  {
    vec <- c()
    for (el in x) {
      vec <- append(vec,f(el))
    }
    return (vec)
  }

  plot(x, aux(RV@r,x), main = "cumulative distribution function")
}

#ex6



#' dispersionCompute
#'
#' Dispersion compute for random variable g(X)
#' @param x Repartition to compute
#' @param g Repartition function
#' @keywords VA
#' @export
#' @examples
#' dispersionCompute(RV, i, j)
dispersionCompute <- function(x, g)
{
  return (integrate(g, lower = 1, upper = Inf)$value)
}

#media <- mean(x)

#dispersionCompute(x);

#ex5
x <- rnorm(100)

#' momentsCompute
#'
#' This function compute for a repartition (x) initial moment and centered moment, from ord 1 to 4.
#' @param x Repartition to compute
#' @keywords VA
#' @export
#' @examples
#' momentsCompute(rnorm(100))
momentsCompute <- function(x)
{
  i <- 1
  while (i <= 4) {
    tryCatch(
      {
        firstMomentCompute(x, i)
      },
      error = function(exception) {
        print(paste("Nu exista momentul initial de ordin ", ord))
      }
    )
    tryCatch(
      {
        centeredMomentCompute(x, i)
      },
      error = function(exception) {
        print(paste("Nu exista momentul centrat de ordin ", ord))
      }
    )
    i = i + 1
  }
}

#' firstMomentCompute
#'
#' This function compute for a repartition (x) initial moment, for a specified order.
#' @param x Repartition to compute
#' @param ord Order moment
#' @keywords VA
#' @export
#' @examples
#' firstMomentCompute(rnorm(100), 1)
firstMomentCompute <- function(x, ord)
{
  return (moment(x, order=ord))
}

#' centeredMomentCompute
#'
#' This function compute for a repartition (x) centered moment, for a specified order.
#' @param x Repartition to compute
#' @param ord Order moment
#' @keywords VA
#' @export
#' @examples
#' centeredMomentCompute(rnorm(100), 1)
centeredMomentCompute <- function(x, ord) {
  return (moment(x, order=ord, center=TRUE))
}
