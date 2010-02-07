\name{Dotplot}
\alias{Dotplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dotplot
}
\description{
Histogram like charts for one or two quantitative variables in a small data set. Stacked dots (one per each data point)
are used in place of bars to indicate the frequency of with which each data value appears in the set. 
}
\usage{
Dotplot(x, y = NULL,  xlim = NULL, col = NULL, xlab, dlab,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  First or only quantitative variable for dotplot
}
  \item{y}{
  If specified second quantitative variable for dotplot, both plots show on same page
}
  \item{xlim}{
  Vector of low, high values for x-axis
}
  \item{col}{
  Color of dots for single variable, vector of dot colors for two variables
}
  \item{xlab}{
  Label or vector of labels for the x-axe(s)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
