% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traversal.R
\name{synPluck}
\alias{synPluck}
\title{Traverse a synapse path}
\usage{
synPluck(sid, ...)
}
\arguments{
\item{sid}{Synapse ID of the starting entity}

\item{...}{One or more names for constructing a path. Accepts individual names, vectors and lists}
}
\value{
Synapse ID of the "plucked" entity
}
\description{
Starting from the provided synapse ID, traverses descendants by name
}
\examples{
\dontrun{
synPluck( "syn1773110", "mRNA", "Counts", "htseq-count", "H9.144.7.7.txt" )
# [1] "syn2822560"
}
}
