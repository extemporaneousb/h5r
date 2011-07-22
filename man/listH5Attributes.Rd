\name{listH5Attributes}
\alias{listH5Attributes}
\title{
  Lists all H5 Attributes
}
\description{
  'listH5Attributes' is a function that returns the names of all H5 attributes. 
}
\usage{
  listH5Attributes(h5Obj)
}
\arguments{
  \item{h5Obj}{
    H5 object to which the attributes are attached.
}
}
\value{
  \item{attrName}{Name of attribute}
}
\seealso{
  \link{createH5Attributes}
}
\examples{
  require(h5r)
  h5 <- H5File("test.h5", 'w')
  createH5Attribute(h5, "attri", c("a","b","c","d"))
  createH5Attribute(h5, "bute", as.integer(c(rnorm(1),rpois(1,5))))
}
\keyword{attribute}

