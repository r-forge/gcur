\name{Quantile}
\alias{Quantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Quantile(x, probs = seq(0, 1, 0.25), na.rm, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{probs}{
%%     ~~Describe \code{probs} here~~
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
function (x, probs = seq(0, 1, 0.25), na.rm, ...) 
{
    l = 0
    if (missing(na.rm)) {
        l = sum(is.na(x))
        na.rm = T
    }
    if (l) {
        if (is.data.frame(x) | is.matrix(x)) {
            n = probs * 100
            qtls = c()
            rn = c()
            rows = 0
            for (i in 1:dim(x)[2]) {
                if (is.numeric(x[, i])) {
                  y = colnames(x)[i]
                  rows = rows + 1
                  ll = sum(is.na(x[, i]))
                  if (ll) {
                    if (is.null(y)) {
                      y = sprintf("column \%d of \%s", i, as.character(substitute(x)))
                      cat(sprintf("Warning: \%s has \%d missing values.\n", 
                        y, ll))
                    }
                    else cat(sprintf("Warning: column \%s has \%d missing values.\n", 
                      y, ll))
                  }
                  if (is.null(y)) 
                    rn = c(rn, sprintf("Col \%d:", i))
                  else rn = c(rn, y)
                  q = quantile(x[, i], probs, na.rm, ...)
                  qtls = c(qtls, q)
                }
            }
            qtls = matrix(qtls, nrow = rows, byrow = T)
            cn = c()
            for (i in 1:length(n)) {
                cn = c(cn, sprintf("\%d\%\%", n[i]))
            }
            qtls = as.table(qtls)
            dimnames(qtls) = list(rn, cn)
            return(qtls)
        }
        else {
            z = as.character(substitute(x))
            z = reAss(z)
            cat(sprintf("Warning: \%s has \%d missing values.\n", 
                z, l))
        }
        return(quantile(x, probs, na.rm, ...))
    }
    quantile(x, probs, na.rm, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
