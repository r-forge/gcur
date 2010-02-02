\name{Min}
\alias{Min}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Min
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Min(x, na.rm)
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
            x1 = as.character(substitute(x))
            cmd = sprintf("getResultTable(\%s,min)", x1)
            result = eval(parse(text = cmd))
            return(result)
        }
        else {
            z = as.character(substitute(x))
            z = reAss(z)
            cat(sprintf("Warning: \%s has \%d missing values.\n", 
                z, l))
            return(min(x, na.rm = T))
        }
    }
    min(x, na.rm = na.rm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
