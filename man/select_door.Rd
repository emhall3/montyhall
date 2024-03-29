% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/montyhallproblem.R
\name{select_door}
\alias{select_door}
\title{Select a door for the Monty Hall Problem.}
\usage{
select_door()
}
\arguments{
\item{Doors}{is a numeric vector used to identify each door.}

\item{Size}{The default size for this game is 1.}

\item{Replace}{is set to False so results are not written over.}
}
\value{
The output returns one of three possible door values. This is the
contestant's initial door choice.
}
\description{
\code{select_door()} is a function that returns a list of door values
set to the size you specify.
}
\details{
This function works by drawing from a random sample of three numeric
values and selecting one of these possible values as the contestant's
initial door choice.
}
\examples{
  select_door()
   
}
