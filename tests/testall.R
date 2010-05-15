require(h5r)

assertError <- function(expr) {
  tryCatch({{expr}; FALSE}, simpleError = function(e) {
    return(TRUE)
  })
}

TestHarness <- function() {
  tests <- list()

  getTime <- function(elt) {
    elt[["time"]][3]
  }
  getResult <- function(elt) {
    elt[["result"]]
  }
  printResults <- function() {
    mwidth <- max(nchar(names(tests))) + 5
    fmtString <- paste("\t%-", mwidth, "s %-10g %-10s\n", sep = "")

    cat(sprintf("%s Results for %d tests %s \n\n", paste(rep("-", 30), collapse = ""), length(tests),
                paste(rep("-", 30), collapse = "")))
    
    for (elt in names(tests)) {
      cat(sprintf(fmtString, elt, getTime(tests[[elt]]), getResult(tests[[elt]])))
    }
  }
  
  function(nm, test, action = c("test", "print", "throw")) {
    action <- match.arg(action)
    switch(action,
           test = {
             tm <- system.time({
               b <- tryCatch(test, simpleError = function(e) {
                 return(FALSE)
               })
             })
             tests[[nm]] <<- list("result" = b, "time" = tm)
           },
           print = {
             printResults()
           },
           throw = {
             errs <- ! sapply(tests, getResult)
             if (any(errs)) {
               stop(simpleError(paste("Tests in error:\n", paste(paste("\t", names(tests)[errs], sep = ""), collapse = "\n"),
                                      sep = "")))
             }
           })
  }
}

##
## Make a new TestHarness.
##
TH <- TestHarness()

##
## The tests.
##
file <- system.file("h5_files", "ex_1.h5", package = 'h5r')

## ex_1
f <- H5File(file)
g <- getH5Group(f, "group_1")

TH("group name", g@name == "group_1")

ds1 <- getH5Dataset(g, "ds_1", inMemory = T)

TH("ds_1 dim, 1", all(dim(ds1) == c(1000, 10)))
TH("ds_1 dim, 2", all(dim(ds1[1:10, 1:10]) == c(10, 10)))
TH("ds_1 dim, 3", all(dim(ds1[1:10, ]) == c(10, 10)))
TH("ds_1 dim, 4", is.null(dim(ds1[, 1])))
TH("ds_1 dim, 5", assertError(ds1[,1:12]))

## string dataset
ds2M <- getH5Dataset(g, "ds_2", inMemory = T)
ds2 <- getH5Dataset(g, "ds_2", inMemory = F)

TH("ds_2 dim, 1", all(ds2[] == ds2M[]))
TH("ds_2 dim, 2", all(ds2[1:5] == ds2M[1:5]))

## attributes
a <- getH5Attribute(ds2, "x")
b <- getH5Attribute(ds2, "y")
c <- getH5Attribute(ds2, "z")

TH("attribute 1", all(a[] == 1:3))
TH("attribute 2", all(b[] == rbind(1:3, 5:7)))
TH("attribute 3", all(c[] == ds2[]))

## > 2 dimensional data.
ds3M <- getH5Dataset(g, "ds_3", inMemory = T)
ds3 <- getH5Dataset(g, "ds_3", inMemory = F)

TH("ds_3 dim", all(dim(ds3[,,]) == dim(ds3)) && all(dim(ds3M[,,]) == dim(ds3M)))

## known inconsistency between two.
TH("In memory inconsistency (!! FIXME !!)", assertError(all(ds3M[] == ds3[])))

## the 3d R object.
id3 <- ds3M@.data$.data

TH("3d consistency, slabbed", all(id3[,,] == ds3[,,]) &
   all(id3[,1,,drop=TRUE] == ds3[,1,,drop=TRUE]) &
   all(id3[1,1,,drop=TRUE] == ds3[1,1,,drop=TRUE]) &
   all(id3[1,,3,drop=TRUE] == ds3[1,,3,drop=TRUE]))

TH("3d consistency, memory", all(id3[,,] == ds3M[,,]) &
   all(id3[,1,,drop=TRUE] == ds3M[,1,,drop=TRUE]) &
   all(id3[1,1,,drop=TRUE] == ds3M[1,1,,drop=TRUE]) &
   all(id3[1,,3,drop=TRUE] == ds3M[1,,3,drop=TRUE]))

TH("3d bounds check 1", assertError(ds3[0:10,,]))
TH("3d bounds check 2", assertError(ds3[,,0:10]))
TH("3d bounds check 3", assertError(ds3[1,2,1:1000]))

## 2 dimensional string dataset.
ds4M <- getH5Dataset(g, "ds_4", inMemory = T)
ds4 <- getH5Dataset(g, "ds_4", inMemory = F)

TH("ds_2 dim", all(dim(ds4[,]) == dim(ds4)) & all(dim(ds4M[,]) == dim(ds4)))

TH("ds_4, memory", (function(n = 100, s = 100) {
  g1 <- gc()[,1]
  a <- replicate(n, {
    replicate(s, getH5Dataset(g, "ds_4", inMemory = FALSE)[1:2,1:2])
  })
  rm(a)
  all(g1 - gc()[,1] <= 0)
})())


##
## More in-depth testing of slicing.
##
ds6 <- getH5Dataset(g, "ds_6", inMemory = FALSE)
ds6M <- getH5Dataset(g, "ds_6", inMemory = TRUE)

TH("ds6, slicing", all(ds6[,] == ds6M[,]) & all(ds6[2:1001] == ds6M[2:1001]))

timeMe <- function(d) {
  k <- 100
  n <- 100
  system.time({
    for (i in seq.int(1, n)) {
      b <- runif(1, 1, nrow(d) - k)
      d[b:(b + k)]
    }
  })[3]
}

## These are *real* upper-bounds on timing.
TH("slab selection, timing", (mean(replicate(10, timeMe(ds6))) < 1))
TH("slab selection, timing -- memory", (mean(replicate(10, timeMe(ds6M))) < 1))

randomSlice <- function(d) {
  dims <- dim(d)

  starts <- floor(runif(length(dims), rep(1, length(dims)), dims))
  ends   <- starts + floor(rexp(length(dims), 1/4))
  ends   <- ifelse(ends > dims, dims, ends)

  lst <- as.list(call("[", d))

  for (i in 1:length(dims)) {
    lst[[i + 2]] <- starts[i]:ends[i]
  }
  eval(as.call(lst))
}

ds7 <- getH5Dataset(g, "ds_7", inMemory = FALSE)
ds7M <- getH5Dataset(g, "ds_7", inMemory = TRUE)

TH("random slice", {
  set.seed(10)
  system.time({a <- replicate(10000, randomSlice(ds7))})
  set.seed(10)
  system.time({b <- replicate(10000, randomSlice(ds7M))})
  all.equal(a,b)
})


TH("list attributes, file", {
  length(listH5Contents(f)) == 14
})

TH("list attributes, group", {
  length(listH5Contents(g)) == 11
})

ds8 <- getH5Dataset(g, "ds_8", inMemory = FALSE)

TH("dim check 1", assertError(ds8[,0:5]))
TH("dim check 2", assertError(ds8[0,1:5]))
TH("dim check 3", assertError(ds8[-1,1:5]))
TH("dim check 4", assertError(ds8[10,1]))
TH("test 0-vs-1 based", all(ds8[1,1:5] == 1:5))

TH(action = "print")
TH(action = "throw")

