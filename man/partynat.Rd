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
    \itemize{
    \item{"IPA2"}   
        {Index of Party Aggregation (Allik 2006)}
    \item{"PNSW"}   
        {Weighted Party (System) Nationalization Score (Bochsler 2010) }
    \item{"PNS10"}  
        {Standardized Party (System) Nationalization Score (Bochsler 2010) }
    \item{"TCI"}    
        {Territorial Coverage Index (Caramani 2004)}
    \item{"IPR2"}
        {Index adjusted for Party size and number of Regions (Caramani 2004)}
    \item{"IPA1"}
        {Indicator of Party Aggregation (Chhibber and Kollman 1998)}
    \item{"IS"}
        {Inflation Score (Cox 1999)}
    \item{"VC"}
        {Variability Coefficient (Ersson, Janda, and Lane 1985)}
    \item{"SWVC"}
        {Standardized and Weighted Variability Coefficient (Ersson, Janda, and Lane 1985)}
    \item{"CPR"}
        {Coefficient of Party Regionalization (Golosov 2016)}
    \item{"IPN"}
        {Index of Party (System) Nationalization (Golosov 2016)}
    \item{"NPNS"}
        {Normalized Party (System) Nationalization Score (Golosov 2016)}
    \item{"NVC"}
        {Normalized Coefficient of Variation (Golosov 2016)}
    \item{"IPR1"}
        {Index of Party Regionalization (Golosov and Ponarin 1999)}
    \item{"PNS"}
        {Party Nationalization Score (Jones and Mainwaring 2003)}
    \item{"Lee"}
        {Lee index (Lee 1988)}
    \item{"II"}
        {Inflation Index (Moenius and Kasuya 2004)}
    \item{"MAD"}
        {Index of variation/Mean Absolute Deviation (Rose and Urwin 1975)}
    \item{"CRI"}
        {Cumulative Regional Inequality (Rose and Urwin 1975)}
    \item{"MSD"}
        {Mean Standard Deviation of row shares}
    \item{"Var"}
        {Variance of row shares}
    \item{"MI"}
        {Mutual Information (Frankel and Volij 2011)}
    \item{"Delta"}
        {Dissimilarity index for choice-group independence (Medzihorsky 2022)}
    }
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
    \item{confidence_level}{ The requested confidence level in case bootstrap, 
        jackknife, or subsampling were performed. }
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
    set.seed(1234)
    tab_1 <- matrix(round(rpois(40, 100)*rbeta(40, 1, 1)),
                    nrow=10, ncol=4)
    rownames(tab_1) <- paste0(1:nrow(tab_1))
    colnames(tab_1) <- LETTERS[1:4]
    head(tab)
    d_1 <- partynat(tab_1, boot=TRUE, n_rep=1e2)
    summary(d_1)
}
\keyword{ partynat }
\keyword{ nationalization }
