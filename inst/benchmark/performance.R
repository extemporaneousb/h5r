##
## Investigate performance profiles of various access patterns.
##
require(h5r)

f <- function(d, n = 100, mu = 1000) {
  start <- runif(n, 1, length(d))
  end   <- start + round(rexp(n, 1/mu))
  end   <- ifelse(end > length(d), start, end)
  width <- end - start + 1
  
  lapply(seq.int(1, n), function(i) {
    readSlab(d, start[i], width[i])
    return(NULL)
  })
  return(TRUE)
}

l <- do.call(rbind, lapply(list(H5File(Sys.glob("./u_big.h5")), H5File(Sys.glob("./z_big.h5"))), function(h5) {
  do.call(rbind, lapply(c("1e3", "1e4", "1e5"), function(s) {
    d <- getH5Dataset(h5, paste("data", s, sep = "_"))
    replicate(100, {
      system.time(f(d, n = 1000))[3]
    })
  }))
}))

write.table(l, file = "rres.dta")


