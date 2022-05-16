\name{summary.partynat}
\alias{summary.partynat}
\title{
	function for summaries of \code{'partynat'} objects
}
\description{
	The function inputs an object of S3 class \code{'partynat'} and returns its summary.
}
\usage{
summary.partynat(x, d = 2, ...)
}
\arguments{
  \item{x}{
	  An object of S3 class \code{'partynat'}.
}
  \item{d}{
	  Numeric, number of digits to print.
}
  \item{\dots}{
	  Further arguments to be passed. Currently unused.
}
}
\value{
	The summary is only printed on the console, i.e., no object is saved.
}
\author{
	Juraj Medzihorsky
}
\seealso{
	\code{\link[partynat::partynat]{partynat}}
}
\keyword{ partynat }
\keyword{ summary }
