% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkArchive.R
\name{checkArchive}
\alias{checkArchive}
\title{checkArchive}
\usage{
checkArchive(path)
}
\arguments{
\item{path}{path to the archive}
}
\description{
\code{checkArchive} checks that index and archive are consistent.
}
\details{
\code{checkArchive} checks that index and archive contents are consistent
and prints details of any inconsistencies.

see \code{\link[=createArchive]{createArchive()}} for details of other archiving functions and of the contents of the index.
}
\examples{
\dontrun{checkArchive("C:/myStore")}

}
