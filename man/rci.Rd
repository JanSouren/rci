\name{rci}
\alias{rci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reliable Change Index
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes the Reliable Change Index for the two columns specified
}
\usage{
rci(df, pretest, posttest, method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Takes a data frame object
}
  \item{pretest}{
  Takes a column name, using "" brackets
  }
  \item{posttest}{
  Takes a column name, using "" brackets
  }
  \item{method}{
  Takes 1 of 5 methods computing the RCI: (a) Jacobson & Truax, 1991,
  (b) Christensen & Mendoza, 1986, (c) Iverson et al., 2003,
  (d) Lewis et al., 2007 [work in progress], or (e) McSweeney et al., 1993.
  If unspecified, default is Jacobson & Truax, 1991
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
# Data frame containing of Pre-test and Post-test columns
set.seed(5)
df <- data.frame(ID = c("AAA", "BBB", "CCC", "DDD", "EEE"), W1 = rnorm(5), W2 = rnorm(5))

# Input the data frame, Pre-test column, and Post-test columns as arguments, respectively
rci(df, "W1", "W2")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
