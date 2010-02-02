\name{CreateDataSet}
\alias{CreateDataSet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CreateDataSet
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateDataSet(setName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{setName}{
%%     ~~Describe \code{setName} here~~
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
function (setName) 
{
    name = NULL
    if (!missing(setName)) {
        name = as.character(substitute(setName))
        check = sprintf("exists('\%s')", name)
        isthere = eval(parse(text = check))
        if (isthere) {
            cat(sprintf("\%s exists in environment, must remove first\n", 
                name))
            return(invisible(NULL))
        }
    }
    x = data.frame()
    if (is.null(name)) 
        return(edit(x))
    action = sprintf("\%s <<- edit(x)", name)
    eval(parse(text = action))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
