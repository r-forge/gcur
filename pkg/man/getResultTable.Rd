\name{getResultTable}
\alias{getResultTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getResultTable
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getResultTable(x, fn, opts1 = NULL, opts2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{fn}{
%%     ~~Describe \code{fn} here~~
}
  \item{opts1}{
%%     ~~Describe \code{opts1} here~~
}
  \item{opts2}{
%%     ~~Describe \code{opts2} here~~
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
function (x, fn, opts1 = NULL, opts2 = NULL) 
{
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
            cmd = sprintf("\%s(\%s[,i],", as.character(substitute(fn)), 
                as.character(substitute(x)))
            if (!is.null(opts1)) 
                cmd = sprintf("\%s \%s,", cmd, opts1)
            cmd = sprintf("\%s na.rm=T", cmd)
            if (!is.null(opts2)) {
                cmd = sprintf("\%s,\%s)", cmd, opts2)
            }
            else {
                cmd = sprintf("\%s)", cmd)
            }
            results[j] = eval(parse(text = cmd))
        }
    }
    if (j) {
        results = as.table(results)
        cmd = sprintf("dimnames(results)=list(\%s=rn)", as.character(substitute(fn)))
        eval(parse(text = cmd))
        return(results)
    }
    else {
        cat(sprintf("\%s has no numeric columns\n", as.character(substitute(x))))
        return(0)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
