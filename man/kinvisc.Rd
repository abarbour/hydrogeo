\name{kinvisc}
\alias{kinvisc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kinvisc(dynamic_fluid_visc = 0.001002, fluid_dens = 1000, grav.divide = FALSE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dynamic_fluid_visc}{
%%     ~~Describe \code{dynamic_fluid_visc} here~~
}
  \item{fluid_dens}{
%%     ~~Describe \code{fluid_dens} here~~
}
  \item{grav.divide}{
%%     ~~Describe \code{grav.divide} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (dynamic_fluid_visc = 0.001002, fluid_dens = 1000, grav.divide = FALSE, 
    verbose = FALSE) 
{
    mu. <- dynamic_fluid_visc
    rho. <- fluid_dens
    nu. <- mu./rho.
    if (grav.divide) {
        nu. <- nu./9.81
        if (verbose) 
            message("kinem-visc. [L**2/T] divided by grav.; units are now [L*T]")
    }
    return(nu.)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
