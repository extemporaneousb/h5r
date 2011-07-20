\name{createH5Dataset}
\alias{createH5Dataset}
\alias{writeH5Data}

\title{Create a Dataset in the Specified H5 Object}
\description{
  'createH5Dataset' contains a header and a data
  array. The header describes the name of the group, datatype,
  dataspace, and storage layout.
}
\usage{
  createH5Dataset(h5Obj, datasetName, data, dims, dType =
  c("integer", "double", "character"), chunkSizes = NA, overwrite = TRUE)
}
\arguments{
  \item{h5Obj}{
    H5 object in which the dataset is located.
  }
  \item{datasetName}{
    Specify name of dataset.
  }
  \item{data} {
    The actual data to store, e.g., a matrix or vector of the same data type.
  }
  \item{dims}{
    Specify dimensions of dataset.
  }
  \item{dType}{
    Specify data type; integer, double or character.
  }
  \item{chunkSizes} {
    Specify chunk sizes of dataset.
  }
  \item{overwrite} {
    Overwrite dataset.
    
  }
  \item{offsets} {
    Determines x,y-point at which vector or matrix begins.
  }
  \item{extents} {
    Determines the length of the vector or matrix.
    }
}
\value{
  Values returned are integer, double or character, depending on
  data type.
}
\examples{
  library("h5r")
  h5 <- H5File("test.H5",'w')
  m <- createH5Dataset(h5, "mm", cbind(rnorm(1000), rnorm(1000)))
  m[1:10, 2]

  m <- createH5Dataset(h5, "nn", dims = c(2,2),dType = "character")
  m[]
  writeH5Data(m, c("agct", "tcga"), c(1,1), c(1,2)) 
  writeH5Data(m, c("jack", "jill"), c(2,1), c(1,2)) 
  m[]
  ls(h5)
  m <- createH5Dataset(h5, "vect", dims = c(10000,1), dType = "integer")
  m[]
  for(i in 1:10) {
      writeH5Data(m, as.integer(rpois(1000,lambda = 10)), c((i-1)*1000+1,1),c( 1000,1))
  }
}
\keyword{datasets}

