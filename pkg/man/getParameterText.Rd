\name{getParameterText}
\alias{getParameterText}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getParameterText
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getParameterText(invoke, pname, ppos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{invoke}{
%%     ~~Describe \code{invoke} here~~
}
  \item{pname}{
%%     ~~Describe \code{pname} here~~
}
  \item{ppos}{
%%     ~~Describe \code{ppos} here~~
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
function (invoke, pname, ppos) 
{
    callList = as.list(invoke[[1]])
    callText = as.character(invoke[[1]])
    pnames = names(callList)
    pos = ((1:length(pnames))[pnames == pname])[1]
    if (!is.na(pos)) 
        return(callText[pos])
    else return(callText[ppos + 1])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
