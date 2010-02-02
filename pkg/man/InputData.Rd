\name{InputData}
\alias{InputData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
InputData
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
InputData(form = ask, guided = F, help = F, sep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{
%%     ~~Describe \code{form} here~~
}
  \item{guided}{
%%     ~~Describe \code{guided} here~~
}
  \item{help}{
%%     ~~Describe \code{help} here~~
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
function (form = ask, guided = F, help = F, sep) 
{
    if (missing(form)) {
    }
    type = as.character(substitute(form))
    if (type == "ask") {
        cat("Choices are\n")
        cat("   1) vector ---- inputData(vec)")
        cat("   2) data set -- inputData(set)")
        cat("   3) 1-d table - inputData(t1)")
        cat("   4) 2-d table - inputData(t2)")
        prompt = "enter 1-4 to run, return to exit"
        choice = readline(prompt)
        type = ""
        if (choice == "1") 
            type = "vec"
        if (choice == "2") 
            type = "set"
        if (choice == "3") 
            type = "t1"
        if (choice == "4") 
            type = "t2"
        if (type == "") 
            invisible(return())
    }
    if (missing(sep) & (type == "vec")) 
        sep = " "
    else sep = defsep
    if (type == "vec") 
        return(scan(sep = sep))
    if (type == "set") 
        return(createDataSet())
    if ((type == "tabl1") | (type == "t1")) {
        return(inputT1(guided, help, sep))
    }
    if ((type == "tabl2") | (type == "t2")) {
        return(inputT2(guided, help, sep))
    }
    cat(paste("Error: ", type, " is not a valid type\n"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
