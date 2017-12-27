
# Simulation of Monte Carlo Integration

monte_carlo1 <- function(fn, x.min, x.max, y.min, y.max, n){
  In.sum <- 0
  for (i in 1:n){
    X <- runif(1,x.min, x.max)
    Y <- runif(1, y.min, y.max)
    In <- (Y <= fn(X))
    In.sum <- In.sum + In
  }
  I <- (In.sum/n)*(x.max - x.min)*(y.max - y.min) + (x.max - x.min)*y.min
  return(I)
}

# Test for Integral x^2 with x.min = 0 and x.max = 1

f <- function(x) x**2

Int_xsq <- monte_carlo1(f, 0, 1, 0, 2, 100000)


Int_ex <- integrate(f, 0, 1)


# Monte Carlo 2, partially vectorised version of 1

monte_carlo2 <- function(fn, x.min, x.max, y.min, y.max, n){
  X <- runif(n,x.min, x.max)
  Y <- runif(n, y.min, y.max)
  In <- (Y <= sapply(X, fn))
  I <- (cumsum(In)/(1:n))*(x.max - x.min)*(y.max - y.min) + (x.max - x.min)*y.min
  plot(1:n, I, type = "l", main = "Monte Carlo Integration", ylab = "I[n]")
  return(I[n])
}

Int_xsq2 <- monte_carlo2(f, 0, 1, 0, 2, 100000)

Int_xsq2a <- monte_carlo2(f, 0, 1, 0, 2, 100000)

# Monte Carlo 3
monte_carlo3 <- function(fn, x.min, x.max, n){
  u <- runif(n, x.min, x.max)
  x <- sapply(u, fn)
  return(mean(x)*(x.max - x.min))
}

Int_xsq3 <- monte_carlo3(f, 0, 1, 100000)
Int_xsq3a <- monte_carlo3(f, 0, 1, 100000)

plot(f)

#?sapply

fb <- function(x) (1-x)/sqrt(x)
Int_fb <- monte_carlo3(fb, 0, 1, 100000)
Int_ex_fb <- integrate(fb, 0, 1)
