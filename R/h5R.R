## ##############################################################################
##
## h5R.R : Main interface file to the hdf5 C libraries.
##
## ##############################################################################

## These are defined in H5Tpublic.h
.h5Types <- c("integer", "numeric", "time", "character", "logical")

setClass("H5Obj", representation(ePtr = "externalptr"))
setClass("H5File", contains = "H5Obj", representation(fileName = "character"))
setClass("H5Group", contains = "H5Obj", representation(name = "character"))

setClassUnion("envOrNULL", c("environment", "NULL"))
setClass("H5DataContainer", contains = "H5Obj",
         representation(name = "character", dims = "integer",
                        h5Type = "integer", .data = "envOrNULL"))
setClass("H5Dataset", contains = "H5DataContainer")
setClass("H5Attribute", contains = "H5DataContainer")

setClass("hSlab", representation = representation(s = "integer", w = "integer"))

hSlab <- function(start, width = NA, end = NA) {
  stopifnot(length(start) == length(width) || length(start) == length(end))

  if (any(is.na(end)) & any(is.na(width))) {
    stop("Must specify either end or width.")
  } else {
    if(any(is.na(end))) {
      width <- width
    }
    else {
      width <- (end - start + 1)
    }
  }
  obj <- new("hSlab")
  obj@s <- as.integer(start)
  obj@w <- as.integer(width)
  return(obj)
}

setMethod("length", "hSlab", function(x) {
  length(x@s)
})
setGeneric("start", function(x, ...) {
  standardGeneric("start")
})
setMethod("start", "hSlab", function(x) {
  x@s
})
setGeneric("width", function(x, ...) {
  standardGeneric("width")
})
setMethod("width", "hSlab", function(x) {
  x@w
})


H5File <- function(fileName) {
  new("H5File", fileName)
}

.ePtr <- function(obj) obj@ePtr

.H5Obj <- function(ep) {
  o <- new("H5Obj")
  o@ePtr <- ep
  return(o)
}

.H5Group <- function(ep, name) {
  o <- new("H5Group")
  o@ePtr <- ep
  o@name <- name
  return(o)
}

.hasData <- function(h5DataContainer) {
  return(exists(".data", h5DataContainer@.data))
}

.putData <- function(h5DataContainer, dta) {
  assign(".data", dta, h5DataContainer@.data)
}

.getData <- function(h5DataContainer) {
  get(".data", h5DataContainer@.data)
}

.inMemory <- function(h5Dataset) {
  return(! is.null(h5Dataset@.data))
}

setGeneric("getH5Group", function(h5Obj, groupName, ...) {
  standardGeneric("getH5Group")
})

setGeneric("getH5Dim", function(h5Obj, ...) {
  standardGeneric("getH5Dim")
})

setGeneric("getH5Type", function(h5Obj, ...) {
  standardGeneric("getH5Type")
})

setGeneric("getH5Dataset", function(h5Obj, datasetName, ...) {
  standardGeneric("getH5Dataset")
})

setGeneric("getH5Attribute", function(h5Obj, attrName, ...) {
  standardGeneric("getH5Attribute")
})

setMethod("getH5Group", c("H5Obj", "character"), function(h5Obj, groupName) {
  .H5Group(.Call("h5R_get_group", .ePtr(h5Obj), groupName,
                 PACKAGE = 'h5r'), groupName)
})

setMethod("getH5Dim", "H5DataContainer", function(h5Obj) {
  .Call('h5R_get_dims', .ePtr(h5Obj), PACKAGE = 'h5r')
})

setMethod("getH5Type", "H5DataContainer", function(h5Obj) {
  .Call("h5R_get_type", .ePtr(h5Obj), PACKAGE = 'h5r')
})

setMethod("initialize", c("H5File"), function(.Object, fileName) {
  ## This is obscene. I have to do this because somehow Subclasses
  ## call this at *class* instantiation time. 
  if (missing(fileName))
    return(.Object)
  
  .Object@ePtr <- .Call("h5R_open", fileName, package = "h5R")
  .Object@fileName <- fileName
  return(.Object)
})

.initH5DataContainer <- function(o, name, inMemory) {
  o@name <- name
  o@h5Type <- getH5Type(o)
  o@dims <- getH5Dim(o)
  
  if (! inMemory) {
    o@.data <- NULL
  } else {
    o@.data <- new.env(parent = emptyenv(), hash = TRUE)
  }
  
  return(o)
}

setMethod("getH5Dataset", c("H5Obj", "character"), function(h5Obj, datasetName,
                                                            inMemory = FALSE) {
  o <- new("H5Dataset")
  o@ePtr <- .Call("h5R_get_dataset", .ePtr(h5Obj), datasetName, PACKAGE = 'h5r')
  return(.initH5DataContainer(o, datasetName, inMemory))
})

setMethod("getH5Attribute", c("H5Obj", "character"), function(h5Obj, attrName) {
  o <- new("H5Attribute")
  o@ePtr <- .Call("h5R_get_attr", .ePtr(h5Obj), attrName, PACKAGE = 'h5r')
  return(.initH5DataContainer(o, attrName, inMemory = TRUE))
})

.internalSlice <- function(x, i, j, ..., drop = TRUE) {
  if (!.hasData(x)) {
    .putData(x, .loadDataset(x))
  }
  d <- .getData(x)
  
  if (is.null(dim(x))) {
    if (! missing(j))
      stop("incorrect number of dimensions")
    d[i]
  }
  else {
    d[i, j, ..., drop = drop]
  }
}

.marginCheck <- function(i, d) {
  if (any(i <= 0))
    stop("Non-positive selections not allowed when subsetting H5Datasets")
  if (max(i) > d)
    stop("Index out of range.")
}

.getExtras <- function(kall, dims) {
  d <- match("drop", names(kall))
  if (! is.na(d))
    kall <- kall[-d]

  j <- match("j", names(kall))
  if (! is.na(j))
    kall <- kall[-j]

  i <- match("i", names(kall))
  if (! is.na(i))
    kall <- kall[-i]

  kall <- kall[-(1:2)]

  if (length(kall) != length(dims))
    stop("Incorrect number of dimensions.")
  
  mapply(function(a,b) {
    if (is.call(a) || is.numeric(a) && !(as.character(a) == ""))
      eval(a)
    else
      seq.int(1, b)
  }, kall, dims, SIMPLIFY = FALSE)
}

setMethod("[", "H5DataContainer", .internalSlice)
setMethod("[", "H5Dataset", function(x, i, j, ..., drop = TRUE) {
  iMissing <- TRUE
  if (! missing(i)) {
    iMissing <- FALSE
    .marginCheck(i, nrow(x))
  } 
  
  jMissing <- TRUE
  if (! missing(j)) {
    jMissing <- FALSE
    .marginCheck(j, ncol(x))
  }
  
  if (.inMemory(x)) {
    ## this is a copy of internal slice, if I don't do it this way
    ## then the arg '...' doesn't really stay consistent and I cannot
    ## pass it through to the '[' built-in.
    if (!.hasData(x)) {
      .putData(x, .loadDataset(x))
    }
    d <- .getData(x)
    
    if (is.null(dim(x))) {
      if (! jMissing)
        stop("incorrect number of dimensions")
      d[i]
    }
    else {
      d[i, j, ..., drop = drop]
    }
  }
  else {
    ## One dimensional dataset.
    if (is.null(dim(x))) {
      if (! jMissing)
        stop("incorrect number of dimensions")
      if (! iMissing) {
        dta <- readSlab(x, min(i), max(i) - min(i) + 1)
        
        ## contiguity
        if (any(diff(i) != 1)) {
          dta <- dta[i - min(i) + 1]
        }
      }
      else
        dta <- readSlab(x, 1, length(x))
    }

    ## > 1-D dataset.
    else {

      if (length(dim(x)) > 3) {
        ## Need to call this function w/in this scope, but don't want
        ## to absorb cost if we won't need it.
        kall <- as.list(match.call())
      }
      
      extras <- tryCatch(list(...), simpleError = function(e) {
        if (length(dim(x)) > 3) {
          .getExtras(kall, dim(x)[-(1:2)]) # remove i,j
        } else {
          return(list())
        }
      })

      nExtra <- 0
      if (length(extras) > 0) {
        for (k in 3:(2 + length(extras))) {
          nExtra <- nExtra + 1
          .marginCheck(extras[[k-2]], dim(x)[k])
        }
      }
       
      ## need to specify the range to select.
      sel <- matrix(NA, nrow = length(dim(x)), ncol = 2)
      lst <- list(`[`, x = quote(dta))
      
      if (! iMissing) {
        sel[1, ] <- range(i)
        lst$i <- i - min(i) + 1
      } else {
        ## retain original dimensions.
        sel[1, ] <- c(1, dim(x)[1])
        lst$i <- seq.int(sel[1,1], sel[1,2])
      }

      if (! jMissing) {
        sel[2, ] <- range(j)
        lst$j <- j - min(j) + 1
      } 
      else {
        ## retain original dimensions.
        sel[2, ] <- c(1, dim(x)[2])
        lst$j <- seq.int(sel[2,1], sel[2,2])
      }

      if (nrow(sel) > 2) {
        for (k in 3:nrow(sel)) {
          if (length(extras) >= k - 2) {
            sel[k, ] <- range(extras[[k - 2]]) # the offset into the list.
            lst[[k+2]] <- extras[[k-2]] - min(extras[[k-2]]) + 1
          }
          else {
            sel[k, ] <- c(1, dim(x)[k])
            lst[[k+2]] <- seq.int(sel[k,1], sel[k,2])
          }
        }
      }
      ext <- sel[,2] - sel[,1] + 1
      dta <- readSlab(x, sel[,1], ext)

      ## Now I have to fix things up because of the contiguity
      ## issue. Essentially, if the i, j, ... specified by the user
      ## aren't contiguous then I have to subselect the dta to conform
      ## to their selection.
      dta <- eval(as.call(lst))
    }
    dta <- if (drop) drop(dta) else dta

    ## This is so dim(x[]) matches dim(x).
    if (is.null(dim(x)))
      as.vector(dta)
    else
      dta
  }
})

## This function is written to leverage the possibility of fast contiguous
## range access.
setMethod("[", c("H5Dataset", "hSlab", "missing", "missing"), function(x, i) {
  if (.inMemory(x))
    stop("Not implemented for inMemory datasets.")
  
  nr <- length(i)
  if (! ((nr == 1 && is.null(dim(x))) || (nr == length(dim(x)))))
    stop("Dimension mismatch: nrow(x) == length(dim(x))")
  
  readSlab(x, start(i), width(i))
})

##
## Note: the two reverses.
##
.loadDataset <- function(h5Dataset) {
  d <- readDataAsVector(h5Dataset)
  dim(d) <- rev(dim(h5Dataset))
  
  if (! is.null(dim(h5Dataset))) aperm(d) else d
}

readSlab <- function(h5Dataset, offsets, dims) {
  if (! all((offsets + dims - 1) <= dim(h5Dataset)))
    stop("error invalid slice specification in readSlab.")
  
  d <- .Call("h5R_read_slab", .ePtr(h5Dataset), as.integer(offsets - 1), as.integer(dims))
  dim(d) <- rev(dims)

  if (! is.null(dim(h5Dataset))) aperm(d) else d
}

setGeneric("readDataAsVector", function(h5Obj, ...) {
  standardGeneric("readDataAsVector")
})

setMethod("readDataAsVector", "H5Dataset", function(h5Obj) {
  .Call('h5R_read_dataset', .ePtr(h5Obj), PACKAGE = 'h5r')
})

setMethod("readDataAsVector", "H5Attribute", function(h5Obj) {
  .Call('h5R_read_attr', .ePtr(h5Obj), PACKAGE = 'h5r')
})

setMethod("show", "H5Obj", function(object) {
  cat("class of:", class(object), "\n")
})

setMethod("show", "H5File", function(object) {
  callNextMethod(object)
  cat("file:", object@fileName, "\n")
})

setMethod("show", "H5Group", function(object) {
  callNextMethod(object)
  cat("name:", object@name, "\n")
})

.getTypeString <- function(h5Dataset) {
  .h5Types[h5Dataset@h5Type + 1]
}

setMethod("show", "H5DataContainer", function(object) {
  callNextMethod(object)
  cat("name:", object@name, "\n")
  cat("dim: ", object@dims, "\n")
  cat("type:", .getTypeString(object), "\n")
})

setMethod("dim", "H5DataContainer", function(x) if (length(x@dims) < 2) NULL else x@dims)
setMethod("length", "H5DataContainer", function(x) if (is.null(dim(x))) x@dims else prod(x@dims))
setMethod("nrow", "H5DataContainer", function(x) {
  x@dims[1]
})
setMethod("ncol", "H5DataContainer", function(x) {
  x@dims[2]
})


##
## Examining the file contents.
##

## construct a list of elements in the file.
.listH5Contents <- function(h5Obj) .Call("h5R_list_contents", .ePtr(h5Obj))
.listH5Attributes <- function(h5Obj) .Call("h5R_list_attributes", .ePtr(h5Obj))

listH5Contents <- function(h5Obj) {
  contents <- .listH5Contents(h5Obj)
  
  lst <- lapply(contents, function(a) {
    h5Obj <- switch(as.character(a[[2]]),
                    '0' = { getH5Group(h5Obj, a[[1]]) },
                    '1' = { getH5Dataset(h5Obj, a[[1]]) })

    if (class(h5Obj) == "H5Dataset") {
      dim <- getH5Dim(h5Obj)
    } else {
      dim <- NA
    }
    list(name = a[[1]], type = a[[2]], attributes = .listH5Attributes(h5Obj),
         dim = dim)
  })
  names(lst) <- sapply(lst, "[[", 1)
  return(lst)
}

##
## Does the name exist directly below the h5Obj.
##
## XXX: This function might be implemented in C more
##      efficiently, i.e., short-circuiting when the
##      object is found, but I have avoided using the
##      Lightweight interface.
h5Exists <- function(h5Obj, name) {
  a <- .listH5Contents(h5Obj)
  n <- sapply(a, "[[", 1)
  s <- sapply(strsplit(n, "/"), "[[", 1)
  any(s == name)

  ## This call determines if an object exists anywhere in
  ## the file with 'name'
  ##
  ## return(.Call("h5R_name_exists", .ePtr(h5Obj), name))
}

  


