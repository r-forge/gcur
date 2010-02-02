\name{GetTable2}
\alias{GetTable2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GetTable2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetTable2(file = file.choose(), header = T, sep = defsep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
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
function (file = file.choose(), header = T, sep = defsep) 
{
    x = readLines(file)
    if (!length(x)) {
        print("no table data")
        return()
    }
    if (header) {
        vnames = unlist(strsplit(x[1], sep))
        if (length(vnames) != 2) {
            cat("Error: Must be two category names")
            return()
        }
        vnames = noquote(noquote(vnames))
        j = 2
    }
    else {
        vnames = c("")
        j = 1
    }
    if (!((length(x) - j) > 1)) {
        cat("Error: no table data")
        return()
    }
    cn = unlist(strsplit(x[2], sep))
    cols = length(cn)
    cols1 = cols + 1
    j = j + 1
    k = 1
    kk = 1
    tabl = c()
    rn = c()
    for (i in j:length(x)) {
        l = unlist(strsplit(x[i], sep))
        if (!(length(l) == cols1) | sum(suppressWarnings(is.na(as.numeric(l[2:cols1]))))) {
            out = paste("Error: invalid data at line ", i)
            cat(out)
            invisible(return())
        }
        rn[k] = noquote(noquote(l[1]))
        tabl[kk:(kk + cols - 1)] = as.numeric(l[2:cols1])
        k = k + 1
        kk = kk + cols
    }
    tabl = matrix(tabl, ncol = cols, byrow = T)
    tabl = as.table(tabl)
    if (length(vnames) == 2) {
        cmd = sprintf("dimnames(tabl)=list(\%s=rn,\%s=cn)", vnames[1], 
            vnames[2])
        eval(parse(text = cmd))
    }
    else dimnames(tabl) = list(rn, cn)
    tabl
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
