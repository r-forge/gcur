\name{InputT1}
\alias{InputT1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
InputT1
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
InputT1(guided = F, help = F, sep = defsep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (guided = F, help = F, sep = defsep) 
{
    if (!guided & help) {
    }
    tabl = c()
    rn = c()
    vname = ""
    linesRead = 0
    k = 1
    xxx = "xxx"
    while (xxx != "") {
        if (!linesRead) {
            prompt = "Enter table variable name "
            xxx = readline(prompt)
            vname = xxx
            xxx = "xxx"
            linesRead = linesRead + 1
        }
        else {
            if (guided) 
                prompt = "Enter value(text), count "
            else prompt = ""
            xxx = readline(prompt)
            if (xxx == "") {
                if (guided) {
                  prompt = "Are you sure you want to terminate data entry? (type n to continue) "
                  xxx = readline(prompt)
                  if ((xxx == "n") | (xxx == "N")) 
                    xxx = "xxx"
                  else xxx = ""
                }
            }
            else {
                yyy = unlist(strsplit(xxx, sep))
                if (length(yyy) != 2) {
                  cat("Data line must contain value text and count separated by commas, please reenter\n")
                }
                else {
                  if (suppressWarnings(is.na(as.numeric(yyy[2])))) {
                    cat("Second entry of data line must be numeric, reenter\n")
                  }
                  else {
                    rn[k] = yyy[1]
                    tabl[k] = as.numeric(yyy[2])
                    k = k + 1
                    linesRead = linesRead + 1
                  }
                }
            }
        }
    }
    if (linesRead == 1) {
        cat("No table data entered\n")
        invisble(return())
    }
    tabl = as.table(tabl)
    if (vname != "") {
        cmd = sprintf("dimnames(tabl)=list(\%s=rn)", vname)
        eval(parse(text = cmd))
    }
    else dimnames(tabl) = list(rn)
    cat(sprintf("\%s data lines read\n", linesRead))
    tabl
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
