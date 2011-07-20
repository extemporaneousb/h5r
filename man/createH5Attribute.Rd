\name{createH5Attribute}
\alias{createH5Attribute}
\title{
  A Function that Creates a Metadata Oject that Describes the Primary Data Object
}
\description{
  'createH5Attribute' describes characteristics of the primary data object,
  such as a dataset, and specifies the attribute property list. The
  primary data object characteristics include the datatype and
  dataspace of the attribute data.
}
\usage{
  createH5Attribute(h5Obj, attrName, attrValue, ...)
}
\arguments{
  \item{h5Obj}{
    Oject to which the attribute is attached.
}
  \item{attrName}{
    Name of attribute.
}
  \item{attrValue}{
    The value portion of the attribute which contains data element(s)
    of the same data type. 
}
   \item{\dots}{
     Extra arguments. See class documentation for specific methods.
}
}
\details{
  The attribute is attached to an object and has a unique name. The
  value part of the attribute conatins one or more data elements.
}
\value{
  An object of type H5Attribute.  
}
\examples{
  require(h5r)
  h5 <- H5File("test.h5", 'w')
  createH5Attribute(h5, "attri", c("a","b","c","d"))
  createH5Attribute(h5, "bute", as.integer(c(rnorm(1),rpois(1,5))))
}
\keyword{attribute}

