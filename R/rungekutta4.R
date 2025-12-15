#' Runge Kutta 4
#'
#' the equation is of the form  a y'' + b y' + c y = F0 cos(wt+p)
#'
#' We use the Runge Kutta 4 method to approximate the solution function
#'
#' @param X the vector containing the coefficients of the equation X = c(a,b,c) / X =c(b,c)
#' @param A the vector constituting the second member A = c(F0,w,p)
#' @param I initial condition vector I = c( t0, y0, y'0 ) / I = c( t0, y0 )
#' @param h time space : by how much does the time change at each iteration
#' @param t the time
#' @param order the order of the equation 2 or 1
#'
#' @returns a 3 or 2 columns table consisting of  t , derive_y , y  for order 2 / t , y for order 1
#' @export
#'
#' @examples
#' table2 <- rungekutta4(c(5,0,10),c(0,0.4,0),c(0,0),0.04,30,2)
#'
#' table1 <- rungekutta4(c(5,-10),c(0,0,0),c(0,6),0.1,5,1)
rungekutta4 <- function (X,A,I,h,t,order){
  F0 <- A[1] ; w <- A[2] ; p <- A[3]

  if (order==1){
    a <- X[1] ; b <- X[2]
    t_0 <- I[1] ; y <- I[2]
    table <- data.frame(t=t_0, y=y)

    while(t_0 < t){
      k1 <- (F0/a)*cos(w*t_0+p) - (b/a)*y
      k2 <- (F0/a)*cos(w*(t_0+h/2)+p) - (b/a)*(y+h*k1/2)
      k3 <- (F0/a)*cos(w*(t_0+h/2)+p) - (b/a)*(y+h*k2/2)
      k4 <- (F0/a)*cos(w*(t_0+h)+p) - (b/a)*(y+h*k3)

      y <- y + (h/6)*(k1+2*k2+2*k3+k4)
      t_0 <- t_0 + h

      point <- c(t_0, y)
      table <- rbind(table,point)
    }
  }

  if (order==2){
    a <- X[1] ; b <- X[2] ; c <- X[3]
    t_0 <- I[1] ; x <- I[2] ; x1 <- X[3]
    table <- data.frame(t=t_0, derive_y= x1, y=x)

    while(t_0 < t){
      k1 <- (F0/a)*cos(w*t_0+p) - (b/a)*x1 - (c/a)*x
      k2 <- (F0/a)*cos(w*(t_0+h/2)+p) - (b/a)*(x1+h*k1/2) - (c/a)*x
      k3 <- (F0/a)*cos(w*(t_0+h/2)+p) - (b/a)*(x1+h*k2/2) - (c/a)*x
      k4 <- (F0/a)*cos(w*(t_0+h)+p) - (b/a)*(x1+h*k3) - (c/a)*x

      x2 <- x1 + (h/6)*(k1+2*k2+2*k3+k4)
      t_0 <- t_0 + h
      ecart <- x2-x1

      k1 <- x2
      k2 <- x2 + ecart*k1/2
      k3 <- x2 + ecart*k2/2
      k4 <- x2 + ecart*k3

      x <- x + (h/6)*(k1+2*k2+2*k3+k4)
      x1 <- x2

      point <- c(t_0, x1, x)
      table <- rbind(table,point)
    }
  }
  return(table)
}
