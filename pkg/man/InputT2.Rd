\name{InputT2}
\alias{InputT2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
InputT2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
InputT2(guided = F, help = F, sep = defsep)
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
    vnames = ""
    linesRead = 0
    k = 1
    kk = 1
    xxx = "xxx"
    while (xxx != "") {
        if (!linesRead) {
            prompt = "Enter table variable names "
            xxx = readline(prompt)
            vnames = unlist(strsplit(xxx, sep))
            if (!length(vnames)) {
                vnames = ""
                linesRead = linesRead + 1
                xxx = "xxx"
            }
            else if (length(vnames) != 2) {
                cat("Error: Must be two or no category names\n")
            }
            else {
                vnames = noquote(noquote(vnames))
                linesRead = linesRead + 1
            }
        }
        else if (linesRead == 1) {
            prompt = sprintf("Enter column names separated by \%s ", 
                sep)
            xxx = readline(prompt)
            if (xxx != "") {
                cn = unlist(strsplit(xxx, sep))
                cols = length(cn)
                cols1 = cols + 1
                linesRead = linesRead + 1
            }
            else {
                xxx = "xxx"
            }
        }
        else {
            if (guided) {
                if (length(vnames)) 
                  startprompt = sprintf("Enter \%s category", 
                    vnames[1])
                else startprompt = "Enter row category "
                prompt = sprintf("\%s & \%d counts ", startprompt, 
                  cols)
            }
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
                yyy = unlist(strsplit(xxx, ","))
                numsOk = T
                if (length(yyy) != cols1) {
                  cat(sprintf("Data line must include category and \%d counts separated by commas, please reenter\n", 
                    cols1))
                }
                else {
                  rn[k] = noquote(noquote(yyy[1]))
                  if (sum(suppressWarnings(is.na(as.numeric(yyy[2:cols1]))))) {
                    cat(sprintf("Non numeric entries found in row \%s.\n", 
                      j - 1))
                  }
                  else {
                    tabl[kk:(kk + cols - 1)] = as.numeric(yyy[2:cols1])
                    k = k + 1
                    kk = kk + cols
                    linesRead = linesRead + 1
                  }
                }
            }
        }
    }
    if (linesRead == 2) {
        cat("No table data entered\n")
        invisble(return())
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
