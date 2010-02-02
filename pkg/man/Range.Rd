\name{Range}
\alias{Range}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Range
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Range(x, na.rm)
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
            j = 0
            results = c()
            rn = c()
            for (i in 1:dim(x)[2]) {
                if (is.numeric(x[, i])) {
                  j = j + 1
                  y = colnames(x)[i]
                  if (is.null(y)) 
                    y = sprintf("col-\%d", i)
                  rn[j] = y
                  ll = sum(is.na(x[, i]))
                  if (ll) {
                    cat(sprintf("Warning: \%s has \%d missing values.\n", 
                      y, ll))
                  }
                  results[j] = max(x[, i], na.rm = T) - min(x[, 
                    i], na.rm = T)
                }
            }
            if (j) {
                results = as.table(results)
                dimnames(results) = list(rn)
                return(results)
            }
            else {
                cat(sprintf("\%s has no numeric columns\n", as.character(substitute(x))))
                return(0)
            }
        }
        z = as.character(substitute(x))
        z = reAss(z)
        cat(sprintf("Warning: \%s has \%d missing values.\n", z, 
            l))
        return(max(x, na.rm = T) - min(x, na.rm = T))
    }
    max(x, na.rm) - min(x, na.rm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
