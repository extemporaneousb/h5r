##
## Investigate performance profiles of various access patterns.
##

require(h5r)


h5 <- H5File("big.h5")

cD <- getH5Dataset(h5, "cdata", inMemory = FALSE)
uD <- getH5Dataset(h5, "udata", inMemory = FALSE)
zD <- getH5Dataset(h5, "zdata", inMemory = FALSE)

f <- function(d, N = 1000, mu = 1000) {
  start <- runif(N, 1, length(d))
  end <- start + round(rexp(N, 1/mu))
  mapply(function(s,e) {
    z <- d[s:e]
  }, start, end)
  1
}

boxplot(as.data.frame(do.call(rbind, sapply(1:10, function(i) {
  set.seed(i)
  cT <- system.time(f(cD))[3]
  uT <- system.time(f(uD))[3]
  zD <- system.time(f(zD))[3]
  list(cT, uT, zD)
}))))
  
  


