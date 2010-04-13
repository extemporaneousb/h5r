.h5Types <- c("integer", "numeric", "character")

setClass("H5Obj", representation(ePtr = "externalptr"))
setClass("H5File", contains = "H5Obj", representation(fileName = "character"))
setClass("H5Group", contains = "H5Obj", representation(name = "character"))
setClass("H5Dataset", contains = "H5Obj",
         representation(name = "character", dims = "integer",
                        h5Type = "character", .data = "environment"))

H5File <- function(fileName) {
  new("H5File", fileName)
}

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

.H5Dataset <- function(ep, name, h5Type, dims) {
  o <- new("H5Dataset")
  o@name <- name
  o@ePtr <- ep
  o@h5Type <- h5Type
  o@dims <- dims

  ## This caches the data. At some point, we'll want to
  ## move away from this and just grab things from disk
  ## and provide a mechanism to cache.
  o@.data <- new.env(parent = emptyenv(), hash = TRUE)

  return(o)
}

.ePtr <- function(obj) obj@ePtr

.hasData <- function(h5Dataset) {
  return(exists(".data", h5Dataset@.data))
}

.putData <- function(h5Dataset, dta) {
  assign(".data", dta, h5Dataset@.data)
}

.getData <- function(h5Dataset) {
  get(".data", h5Dataset@.data)
}

setGeneric("getH5Group", function(h5Obj, groupName, ...) {
  standardGeneric("getH5Group")
})

setGeneric("getH5Dim", function(h5Obj, datasetName, ...) {
  standardGeneric("getH5Dim")
})

setGeneric("getH5Type", function(h5Obj, datasetName, ...) {
  standardGeneric("getH5Type")
})

setGeneric("getH5Dataset", function(h5Obj, datasetName, ...) {
  standardGeneric("getH5Dataset")
})

setGeneric("getH5Attribute", function(h5Obj, attrName, ...) {
  standardGeneric("getH5Attribute")
})

setMethod("getH5Group", "H5Obj", function(h5Obj, groupName) {
  .H5Group(.Call("h5R_get_group", .ePtr(h5Obj), groupName, PACKAGE = 'h5R'),
           groupName)
})

setMethod("getH5Dim", c("H5Obj", "character"), function(h5Obj, datasetName) {
  .Call('h5R_get_dim', .ePtr(h5Obj), datasetName, PACKAGE = 'h5R')
})

setMethod("getH5Type", c("H5Obj", "character"), function(h5Obj, datasetName) {
  i <- .Call("h5R_get_type", .ePtr(h5Obj), datasetName, PACKAGE = 'h5R')
  stopifnot(i > 0)
  .h5Types[i]
})

setMethod("getH5Dataset", c("H5Obj", "character"), function(h5Obj, datasetName) {
  ep <- .Call("h5R_get_dataset", .ePtr(h5Obj), datasetName, PACKAGE = 'h5R')
  dims <- getH5Dim(h5Obj, datasetName)
  h5Type <- getH5Type(h5Obj, datasetName)
  return(.H5Dataset(ep, datasetName, h5Type, dims))
})

setMethod("[", "H5Dataset", function(x, i, j, ..., drop = FALSE) {
  if (!.hasData(x)) {
    .putData(x, .loadDataset(x))
  }
  .getData(x)[i, j, ..., drop = drop]
})

readDataAsVector <- function(h5Dataset) {
  size <- prod(h5Dataset@dims)
  rtype <- which(h5Dataset@h5Type == .h5Types)
  .Call('h5R_read_dataset', .ePtr(h5Dataset), as.integer(size),
        as.integer(rtype), PACKAGE = 'h5R')
}

.loadDataset <- function(h5Dataset) {
  d <- readDataAsVector(h5Dataset)
  dim(d) <- rev(h5Dataset@dims)

  if (length(dim(d)) == 2)
    return(t(d))
  else
    return(d)
}

setMethod("getH5Attribute", c("H5Obj", "character"), function(h5Obj, attrName) {
  .Call('h5R_get_attribute', .ePtr(h5Obj), attrName)
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

setMethod("show", "H5Dataset", function(object) {
  callNextMethod(object)
  cat("name:", object@name, "\n")
  cat("dim: ", object@dims, "\n")
  cat("type:", object@h5Type, "\n")
})

setMethod("dim", "H5Dataset", function(x) x@dims)
setMethod("nrow", "H5Dataset", function(x) x@dims[1])
setMethod("ncol", "H5Dataset", function(x) x@dims[2])
# setMethod("head", "H5Dataset", function(x, n = 10) x[1:n, ])
