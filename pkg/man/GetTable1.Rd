\name{GetTable1}
\alias{GetTable1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GetTable1
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetTable1(file = file.choose(), header = T, sep = defsep)
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
        vname = x[1]
        j = 2
    }
    else {
        vname = ""
        j = 1
    }
    if (!(length(x) - j + 1)) {
        cat("no table data")
        return()
    }
    k = 1
    tabl = c()
    rn = c()
    for (i in j:length(x)) {
        l = unlist(strsplit(x[i], sep))
        if (!(length(l) == 2) | suppressWarnings(is.na(as.numeric(l[2])))) {
            out = paste("invalid data at line ", i)
            cat(out)
            return()
        }
        rn[k] = l[1]
        tabl[k] = as.numeric(l[2])
        k = k + 1
    }
    tabl = as.table(tabl)
    if (vname != "") {
        cmd = sprintf("dimnames(tabl)=list(\%s=rn)", vname)
        eval(parse(text = cmd))
    }
    else dimnames(tabl) = list(rn)
    tabl
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
