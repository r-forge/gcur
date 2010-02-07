\name{Barplot}
\alias{Barplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Barplot
}
\description{
Draws a bar graph with heights supplied as either a vector, matrix, table or two column data frame with bar labels in the first column and bar heights in the second column
If height is a two-dimensional matrix bars are printed in groups by row (note that Barplot handles this differently from the standard R barplot) and by default the bar groups are printed side by side and not in a single tower.
}
\usage{
Barplot(height, freq = T, order = NULL, beside = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{height}{
  height is either a vector, matrix, table, or two column dataframe with bar labels in the first column and numeric values in the second column
  if height is a vector or 1-dimensional table bar heights are taken from the names attribute
  if height is a two-dimensional matrix bars are grouped by row (note this is different from barplot)
}
  \item{freq}{
  if T (default) scale heights to actual frequencies, if F scale heights to relative frequencies
}
  \item{order}{
  only for single dimensional tables
  if "A", "D" order bars in ascending/descending order of height
  if "a", "d" order bars in ascending/descending order of alpha label
}
  \item{beside}{
  if T (default) grouped bars are displayed alongside each other, if F grouped bars are stacked
}
  \item{\dots}{
  see barplot help for remaining parameters
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
Joel Pitt
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
