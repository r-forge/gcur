\name{Max}
\alias{Max}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Max
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Max(x, na.rm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
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
function (x, na.rm) 
{
    l = 0
    if (missing(na.rm)) {
        l = sum(is.na(x))
        na.rm = F
    }
    if (l) {
        if (is.data.frame(x) | is.matrix(x)) {
            invoke = sys.calls()
            x1 = getParameterText(invoke, "x", 1)
            cmd = sprintf("getResultTable(\%s,max)", x1)
            result = eval(parse(text = cmd))
            return(result)
        }
        else {
            invoke = sys.calls()
            z = getParameterText(invoke, "x", 1)
            cat(sprintf("Warning: \%s has \%d missing values.\n", 
                z, l))
            return(max(x, na.rm = T))
        }
    }
    max(x, na.rm = na.rm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
