% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipe_functions.R
\name{ifel_repl}
\alias{ifel_repl}
\title{Replaces values based on if/else boolean logic.
supports piping}
\usage{
ifel_repl(x, boolean, replacement)
}
\value{
Vector
}
\description{
Replaces values based on if/else boolean logic.
supports piping
}
\examples{
c("a","b", "x", "x", "a") \%>\% ifel_repl(. == "x", "replaced!")

}
