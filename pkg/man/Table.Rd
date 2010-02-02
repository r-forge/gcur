\name{Table}
\alias{Table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Table
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Table(x, y = NULL, count = NULL, type = "freq", dnn, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{count}{
%%     ~~Describe \code{count} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{dnn}{
%%     ~~Describe \code{dnn} here~~
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
function (x, y = NULL, count = NULL, type = "freq", dnn, ...) 
{
    if (missing(dnn)) {
        if (is.null(y)) {
            z = as.character(substitute(x))
            z = reAss(z)
            dnn = c(z)
        }
        else {
            z1 = as.character(substitute(x))
            z1 = reAss(z1)
            z2 = as.character(substitute(y))
            z2 = reAss(z2)
            dnn = c(z1, z2)
        }
    }
    if (!is.null(count)) {
        if (is.null(y)) {
            temp = xtabs(count ~ x)
        }
        else {
            temp = xtabs(count ~ x + y)
        }
    }
    else {
        if (is.null(y)) {
            temp = xtable(x, dnn = dnn, ...)
        }
        else {
            temp = xtable(x, y, dnn = dnn, ...)
        }
    }
    z = as.character(substitute(type))
    if (z == "rel") {
        if (is.null(y)) 
            temp = prop.table(temp)
        else temp = prop.table(temp, 1)
    }
    if (z == "perc") {
        if (is.null(y)) 
            temp = prop.table(temp) * 100
        else temp = prop.table(temp, 1) * 100
    }
    temp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
