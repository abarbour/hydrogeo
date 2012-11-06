\name{well_permeab_err}
\alias{well_permeab_err}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
well_permeab_err(Transmiss, Transmiss.stderr, Ls., Ls.stderr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Transmiss}{
%%     ~~Describe \code{Transmiss} here~~
}
  \item{Transmiss.stderr}{
%%     ~~Describe \code{Transmiss.stderr} here~~
}
  \item{Ls.}{
%%     ~~Describe \code{Ls.} here~~
}
  \item{Ls.stderr}{
%%     ~~Describe \code{Ls.stderr} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Transmiss, Transmiss.stderr, Ls., Ls.stderr) 
{
    T. <- Transmiss
    dT. <- Transmiss.stderr
    dLs. <- Ls.stderr
    KVG <- kinvisc(grav.divide = TRUE)
    A. <- dT./Ls.
    B. <- T. * dLs./Ls./Ls.
    dk. <- KVG * norm(cbind(A., B.), type = "F")
    return(dk.)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
