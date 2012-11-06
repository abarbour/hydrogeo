\name{well_permeab}
\alias{well_permeab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
well_permeab(Transmiss, Len_screen, stdErrors = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Transmiss}{
%%     ~~Describe \code{Transmiss} here~~
}
  \item{Len_screen}{
%%     ~~Describe \code{Len_screen} here~~
}
  \item{stdErrors}{
%%     ~~Describe \code{stdErrors} here~~
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
function (Transmiss, Len_screen, stdErrors = NULL) 
{
    KVG <- kinvisc(grav.divide = TRUE)
    Perm. <- Transmiss * KVG/Len_screen
    toret <- list(Permeab = Perm., Permeab.stderr = NA)
    if (!(NA \%in\% stdErrors) & !(is.null(stdErrors))) {
        E. <- as.vector(stdErrors)
        stopifnot(length(E.) == 2)
        toret$Permeab.stderr = well_permeab_err(Transmiss, E.[1], 
            Len_screen, E.[2])
    }
    return(toret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
