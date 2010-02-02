\name{Mean}
\alias{Mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Mean(x, trim = 0, na.rm, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{trim}{
%%     ~~Describe \code{trim} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, trim = 0, na.rm, ...) 
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
            x2 = getParameterText(invoke, "trim", 2)
            if (is.na(x2)) 
                cmd = sprintf("getResultTable(\%s,mean)", x1)
            else if (missing(...)) {
                cmd = sprintf("getResultTable(\%s,mean,\%s)", x1, 
                  x2)
                print(cmd)
            }
            else {
                x3 = as.character(substitute(...))
                cmd = sprintf("getResultTable(\%s,mean,\%s,\%s)", 
                  x1, x2, x3)
                print(cmd)
            }
            result = eval(parse(text = cmd))
            return(result)
        }
        else {
            invoke = sys.calls()
            z = getParameterText(invoke, "x", 1)
            cat(sprintf("Warning: \%s has \%d missing values.\n", 
                z, l))
            return(base::mean(x, trim, na.rm = T, ...))
        }
    }
    mean(x, trim, na.rm, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
