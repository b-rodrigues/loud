% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loudly.R
\name{\%>=\%}
\alias{\%>=\%}
\title{Pipe a loud value to a decorated function}
\usage{
.l \%>=\% .f
}
\arguments{
\item{.l}{A value returned by loudly}

\item{.f}{A loud function to apply to the returning value of .l}
}
\value{
A loud value.
}
\description{
Pipe a loud value to a decorated function
}
\examples{
loud_sqrt <- loudly(sqrt)
loud_exp <- loudly(exp)
3 |> loud_sqrt() \%>=\% loud_exp()
}
