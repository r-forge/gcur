\name{Dotplot}
\alias{Dotplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dotplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Dotplot(x, y = NULL, xlabs = NULL, xlim = NULL, col = NULL, xlab = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{xlabs}{
%%     ~~Describe \code{xlabs} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
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
function (x, y = NULL, xlabs = NULL, xlim = NULL, col = NULL, 
    xlab = NULL, ...) 
{
    if (missing(x)) {
        cat("Error: no first variable specifed\n")
        return(invisible(0))
    }
    if (!is.numeric(x)) {
        cat("Error: first variable must be numeric\n")
        return(invisible(0))
    }
    if (is.null(y)) {
        p1 = xlim
        p2 = col
        if (is.null(p1) & is.null(p2)) 
            stripchart(x, at = 0, pch = 19, method = "stack", 
                frame.plot = F, xlab, ...)
        if (is.null(p1) & !is.null(p2)) 
            stripchart(x, at = 0, pch = 19, method = "stack", 
                frame.plot = F, col = p2, xlab, ...)
        if (!is.null(p1) & is.null(p2)) 
            stripchart(x, at = 0, pch = 19, method = "stack", 
                frame.plot = F, xlim = p1, xlab, ...)
        if (!is.null(p1) & !is.null(p2)) 
            stripchart(x, at = 0, pch = 19, method = "stack", 
                frame.plot = F, xlim = p1, col = p2, xlab, ...)
    }
    if (!is.null(y)) {
        if (!is.numeric(y)) {
            cat("Error: second variable must be numeric\n")
            return(invisble(0))
        }
        if (is.null(xlim)) {
            xlim = c(min(c(x, y), na.rm = T), max(c(x, y), na.rm = T))
        }
        p1 = xlim
        if (is.null(col)) {
            col = c("Green", "Blue")
        }
        p2 = col
        p3 = xlab
        if (!is.null(p3)) {
            if (is.na(p3[2])) 
                p3[2] = ""
        }
        oldpar = par(no.readonly = T)
        par(mfrow = c(2, 1))
        stripchart(x, method = "stack", xlim = p1, col = p2[1], 
            at = 0, pch = 19, frame.plot = F, xlab = p3[1], ...)
        stripchart(y, method = "stack", xlim = p1, col = p2[2], 
            at = 0, pch = 19, frame.plot = F, xlab = p3[2], ...)
        par(oldpar)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
