\name{ImportData}
\alias{ImportData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ImportData
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ImportData(file = file.choose(), header = T, sep = defsep, form = set, ...)
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
  \item{form}{
%%     ~~Describe \code{form} here~~
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
function (file = file.choose(), header = T, sep = defsep, form = set, 
    ...) 
{
    if (missing(form)) 
        return(read.table(file, header, sep, ...))
    type = as.character(substitute(form))
    if (type == "ask") {
    }
    if (missing(sep) & ((type == "vec") | (type == "vech"))) 
        sep = " "
    else sep = defsep
    if (type == "vec") 
        return(scan(file))
    if (type == "vech") 
        return(scan(file, skip = 1))
    if (type == "set") 
        return(read.table(file, header, sep, ...))
    if ((type == "tabl1") | (type == "t1")) 
        return(getTable1(file, header, sep))
    if ((type == "tabl2") | (type == "t2")) 
        return(getTable2(file, header, sep))
    cat(paste("Error: ", type, " is not a valid type\n"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
