\name{partynat}
\alias{partynat}
\title{
	Function to obtain selected party nationalization indices or related quantities.
}
\description{
	This function inputs data in the form of territory-by-choice matrix and outputs the
	value of the selected index for the whole system, and if available, also for each
	choice.  The index can be either selected from those implemented in the package or
	a user-supplied function.  To obtain interval estimates, the user can select from
   	jackknife, nonparametric bootstrap,	and subsampling.		
}
\usage{
partynat(mat, statistic = "PNS", weight_choice = TRUE, weight_territory = TRUE, boot = FALSE, jack = FALSE, subsample = FALSE, n_rep = 10, bias = TRUE, size = round(sum(mat)/2), confidence_level = 0.95)
}
\arguments{
  \item{mat}{
	  A territory-by-choice matrix.
}
  \item{statistic}{
	  Either a character vector indicating the index to use, or a user supplied
	  function. See Details
}
  \item{weight_choice}{
	  Logical, weight choice?  
}
  \item{weight_territory}{
	  Logical, weight territory?
}
  \item{boot}{
	  Logical, perform non-parametric bootstrap?
}
  \item{jack}{
	  Logical, perform jackknife?
}
  \item{subsample}{
	  Logical, perform subsampling?
}
  \item{n_rep}{
	  Numeric, the number of replicates for bootstrap and subsampling.
}
  \item{bias}{
	  Logical, perform bias correction if a resampling procedure is used.
}
  \item{size}{
	 Numeric, subsample size for resampling.
}
  \item{confidence_level}{
	  Numeric, confidence level for interval estimates if a resampling
	  procedure is used.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	An object of S3 class \code{'partynat'}, a list composed of
	\item{call}{ The matched call.}
	\item{stat}{ The statistics argument}
	\item{name}{ The name of the index}
	\item{total}{ Value of the index for the whole table.  If resampling is 
		applied, includes standard errors and confidence intervals.}
	\item{choices}{Values of the index for each choice. NA if not defined under
	   	the selected index. If resampling is 
		applied, includes standard errors and confidence intervals.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	Juraj Medzihorsky
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link[partynat::plot.partynat]{plot.partynat}}
	\code{\link[partynat::summary.partynat]{summary.partynat}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{ partynat }
\keyword{ nationalization }
