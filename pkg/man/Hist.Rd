\name{Hist}
\alias{Hist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hist
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Hist(x, type, xlab, main, breaks = "Sturges", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
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
function (x, type, xlab, main, breaks = "Sturges", ...) 
{
    if (missing(xlab)) {
        z = as.character(substitute(x))
        xl = reAss(z)
    }
    else xl = xlab
    if (missing(main)) 
        mn = paste("Histogram of ", xl)
    else mn = main
    y = as.character(substitute(type))
    if (!is.null(y) & length(y)) {
        if (y == "perc") {
            y = suppressWarnings(hist(x, breaks, plot = F, ...))
            y$counts = 100 * y$counts/sum(y$count)
            y$density = y$counts
            suppressWarnings(plot(y, ylab = "Percent", xlab = xl, 
                main = mn, ...))
            return(invisible(y))
        }
        if (y == "freq") 
            return(hist(x, breaks, freq = T, xlab = xl, main = mn, 
                ...))
        if (y == "dens") 
            return(hist(x, breaks, freq = F, xlab = xl, main = mn, 
                ...))
    }
    hist(x, breaks, xlab = xl, main = mn, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
