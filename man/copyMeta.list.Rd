\name{copyMeta.list}
\alias{copyMeta.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
copyMeta.list(list.dest, list.src)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list.dest}{
%%     ~~Describe \code{list.dest} here~~
}
  \item{list.src}{
%%     ~~Describe \code{list.src} here~~
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
function (list.dest, list.src) 
{
    mapply(function(dest, source) {
        names(dimnames(dest)) <- names(dimnames(source))
        structure(dest, meta = attr(source, "meta"))
    }, list.dest, list.src, SIMPLIFY = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
