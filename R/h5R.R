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

setClass("H5DataContainer", contains = "H5Obj",
         representation(name = "character", dims = "integer",
                        h5Type = "integer", .data = "environment"))
setClass("H5Dataset", contains = "H5DataContainer")
setClass("H5Attribute", contains = "H5DataContainer")

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

setMethod("initialize", "H5File", function(.Object, fileName, ...) {
  ## This is obscene. I have to do this because somehow Subclasses
  ## call this on *class* instanteation time. 
  if (missing(fileName))
    return(.Object)

  .Object@ePtr <- .Call("h5R_open", fileName, package = "h5R")
  .Object@fileName <- fileName
  return(.Object)
})

.initH5DataContainer <- function(o, name) {
  o@name   <- name
  o@dims   <- getH5Dim(o)
  o@h5Type <- getH5Type(o)

  ## This caches the data. At some point, we'll want to
  ## move away from this and just grab things from disk
  ## and provide a mechanism to cache.
  o@.data <- new.env(parent = emptyenv(), hash = TRUE)

  return(o)
}

setMethod("getH5Dataset", c("H5Obj", "character"), function(h5Obj, datasetName) {
  o <- new("H5Dataset")
  o@ePtr <- .Call("h5R_get_dataset", .ePtr(h5Obj), datasetName, PACKAGE = 'h5r')
  return(.initH5DataContainer(o, datasetName))
})

setMethod("getH5Attribute", c("H5Obj", "character"), function(h5Obj, attrName) {
  o <- new("H5Attribute")
  o@ePtr <- .Call("h5R_get_attr", .ePtr(h5Obj), attrName, PACKAGE = 'h5r')
  return(.initH5DataContainer(o, attrName))
})

setMethod("[", "H5DataContainer", function(x, i, j, ..., drop = FALSE) {
  if (!.hasData(x)) {
    .putData(x, .loadDataset(x))
  }
  .getData(x)[i, j, ..., drop = drop]
})

setMethod("[", "H5DataContainer", function(x, i) {
  if (!.hasData(x)) {
    .putData(x, .loadDataset(x))
  }
  if (is.null(dim(x)))
    .getData(x)[i]
  else
    .getData(x)[i,]
})

setGeneric("readDataAsVector", function(h5Obj, ...) {
  standardGeneric("readDataAsVector")
})

setMethod("readDataAsVector", "H5Dataset", function(h5Obj) {
  .Call('h5R_read_dataset', .ePtr(h5Obj), PACKAGE = 'h5r')
})

setMethod("readDataAsVector", "H5Attribute", function(h5Obj) {
  .Call('h5R_read_attr', .ePtr(h5Obj), PACKAGE = 'h5r')
})

.loadDataset <- function(h5Dataset) {
  d <- readDataAsVector(h5Dataset)
  
  if (length(dim(h5Dataset)) == 2) {
    ## I do this because of the column-wise packing of
    ## matrices in R.
    dim(d) <- rev(dim(h5Dataset))
    return(t(d))
  }
  else {
    dim(d) <- dim(h5Dataset)
    return(d)
  }
}

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
setMethod("nrow", "H5DataContainer", function(x) x@dims[1])
setMethod("ncol", "H5DataContainer", function(x) x@dims[2])
