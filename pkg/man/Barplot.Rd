\name{Barplot}
\alias{Barplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Barplot
}
\description{
Draws a bar graph
}
\usage{
Barplot(height, freq = T, order = NULL, beside = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{height}{
  height is either a vector, matrix, table, or two column dataframe with numeric values in the second column
  if height is a vector, 1-dimensional table, or two-column dataframe, 
}
  \item{freq}{
%%     ~~Describe \code{freq} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
}
  \item{beside}{
%%     ~~Describe \code{beside} here~~
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
function (height, freq = T, order = NULL, beside = T, ...) 
{
    if (is.data.frame(height)) {
        if (dim(height)[2] > 2) {
            cat(height)
            cat(" has too many columns\n")
            return()
        }
        if (((dim(height)[2] == 2) & (class(height[, 2]) != "numeric")) | 
            ((dim(height)[2] == 1) & (class(height[, 1]) != "numeric"))) {
            cat(height)
            cat(" does not have numeric data\n")
            return()
        }
        if (dim(height)[2] == 2) {
            height = tapply(height[, 2], height[, 1], sum)
        }
        else height = height[, 1]
    }
    if (!freq) 
        if (!is.null(dim(height))) {
            if (!is.na(dim(height)[2])) 
                height = prop.table(height, 1) * 100
            else height = prop.table(height) * 100
        }
        else height = prop.table(height) * 100
    dim2 = 0
    if (!is.null(dim(height))) 
        if (!is.na(dim(height)[2])) 
            dim2 = 1
    if (!dim2 & !is.null(order)) {
        if (order == "a") 
            height = height[sort(names(height))]
        if (order == "d") 
            height = height[sort(names(height), decreasing = T)]
        if (order == "A") 
            height = height[order(height, height)]
        if (order == "D") 
            height = height[order(height, height, decreasing = T)]
    }
    if (dim2) 
        barplot(t(height), beside = beside, ...)
    else barplot(height, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
