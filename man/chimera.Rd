\name{chimeras}
\alias{chimeraDetectionDenovo}
\alias{chimeraDetectionRef}
\title{
  Detect chimeras
}
\description{
  Detect chimeras de novo and using a reference database.
}
\usage{
  chimeraDetectionDenovo(filename, abskew=2.0, mindiffs=3L, mindiv=0.8, minh=0.28,
                                   nonchimeras=sub(".fasta", "_nonchimera.fasta", filename), 
                                   chimeras=sub(".fasta", "_chimera.fasta", filename), 
                                   borderline=sub(".fasta", "_bordchimera.fasta", filename), 
                                   uchimeout=sub(".fasta", "_chimeraResults.txt", filename))
  %%chimeraDetectionRef()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ Input file }
  \item{abskew}{ Minimum abundance ratio of parent vs chimera (2.0) }
  \item{mindiffs}{ Minimum number of differences in segment (3) }
  \item{mindiv}{ Minimum divergence from closest parent (0.8) }
  \item{minh}{ Minimum score (0.28) }
  \item{borderline}{ Output borderline chimeric sequences to file }
  \item{chimeras}{ Output chimeric sequences to file }
  \item{nonchimeras}{ Output non-chimeric sequences to file }
  \item{uchimeout}{ Output to chimera info to tab-separated file }
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
function (x) 
{
  }
}
