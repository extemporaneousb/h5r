\name{deleteH5Attribute}
\alias{deleteH5Attribute}
\alias{deleteH5Attribute-methods}
\title{
  A Function that Deletes a Metadata Object which Describes the Primary
  Data Object
}
\description{
  'deleteH5Attribute' removes the attribute that describes
  characteristics of the primary data object. 
}
\usage{
 deleteH5Attribute(h5Obj, attrName, ...)
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
\seealso{
  \link[=attribute]{createH5Attribute}
}
\examples{
  require(h5r)
  h5 <- H5File("test.h5", 'w')

  atr <- createH5Attribute(h5, "atr1", 10:1)
  atr

  dl <- deleteH5Attribute(h5, "atr1")
  dl
}
\keyword{datasets}

