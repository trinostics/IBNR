?gamma
n=8
c=gamma((n+1)/2)/sqrt(pi*n)/gamma(n/2)
?integral
?integrate
f <- function(x) {c*(1+(log(x)^2 / n)^((n+1)/2))/x} 
studt <- function(x, n) {
  n1 <- n+1
  c <- gamma(n1/2)/sqrt(n*pi)/gamma(n/2)
  c*(1 + x^2 / n)^(-n1/2)
}
xstudt <- function(x, n) {
  n1 <- n+1
  c <- gamma(n1/2)/sqrt(n*pi)/gamma(n/2)
  x * c*(1 + x^2 / n)^(-n1/2)
}
x2studt <- function(x, n) {
  n1 <- n+1
  c <- gamma(n1/2)/sqrt(n*pi)/gamma(n/2)
  x^2 * c*(1 + x^2 / n)^(-n1/2)
}
integrate(studt, -2, 2, n=8)
integrate(studt, -Inf, Inf, n=8)
integrate(xstudt, -2, 2, n=8)
integrate(xstudt, -Inf, Inf, n=8)
integrate(x2studt, -2, 2, n=8)
integrate(x2studt, -Inf, Inf, n=8)
n/(n-2)

logstudt <- function(x, n) {
  lnx <- log(x)
  n1 <- n+1
  c <- gamma(n1/2)/sqrt(n*pi)/gamma(n/2)
  c*(1 + lnx^2 / n)^(-n1/2) / x
}
xlogstudt <- function(x, n){
  lnx <- log(x)
  n1 <- n+1
  c <- gamma(n1/2)/sqrt(n*pi)/gamma(n/2)
  c*(1 + lnx^2 / n)^(-n1/2)
}
integrate(logstudt, .5, 2, n=8)
integrate(logstudt, 0, Inf, n=8)
integrate(xlogstudt, .5, 2, n=8)
integrate(xlogstudt, 0, 1e6, n=8)
integrate(xlogstudt, 0, 1e8, n=8)
?studentt
sapply(1:12, function(i) integrate(xlogstudt, 0, 10^i, n=8)$valyue) #explodes

# set upper bound = .999 percentile
integrate(xlogstudt, 0, exp(qt(.99999, n)), n=8)

