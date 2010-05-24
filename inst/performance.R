##
## Investigate performance profiles of various access patterns.
##
require(h5r)

h5 <- H5File(Sys.glob("~/h5_files/big.h5"))

cD <- getH5Dataset(h5, "cdata", inMemory = FALSE)
uD <- getH5Dataset(h5, "udata", inMemory = FALSE)
zD <- getH5Dataset(h5, "zdata", inMemory = FALSE)
mD <- getH5Dataset(h5, "cdata", inMemory = TRUE)

N <- 10
K <- 1000

f <- function(d, n = N, mu = 1000) {
  start <- runif(n, 1, length(d))
  end <- start + round(rexp(n, 1/mu))

  mapply(function(s,e) {
    z <- d[s:e]
  }, start, end)

  return(TRUE)
}

par(mar=c(10, 5, 3, 3))
boxplot(as.data.frame(do.call(rbind, lapply(1:K, function(i) {
  sapply(list("chunked" = cD, "unchunked" = uD, "zipped" = zD, "memory" = mD), function(a) {
    set.seed(i)
    system.time(f(a))[3]/N
  })
}))), las = 2)

  


