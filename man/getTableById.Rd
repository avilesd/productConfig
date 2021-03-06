% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetFunctions.R
\name{getTableById}
\alias{getTableById}
\title{Extract the part of the provided dataset that corresponds to a user id.}
\usage{
getTableById(dataset, userid = NULL, ...)
}
\arguments{
\item{dataset}{the data.frame (table) you want to separate in a smaller
data.frame for a specific user.}

\item{userid}{an integer vector, that identifies which rows (from which user) of
the whole given dataset should be extracted. This parameter is vectorised,
you may enter more \code{userid} at once.}
}
\value{
A named list of data.frames with the same columns as the original
  data.frame, but the information on the rows are only for each user.
}
\description{
With \code{userid} you select the rows from one user, all given columns of
\code{dataset} remain intact for that user. We assume the given dataset was
generated by data from a product configurator, and therefore has at least one
column with users' ids. Other columns may include, but are not limited to,
attribute IDs, round, selected attribute and selectable attribute.
}
\examples{
#Not Runnable yet
getTableById(camera_data, 9:25)
getTableById(as.data.frame(matrix_full), 12)
}
\seealso{
Other GetFunctions: \code{\link{benefitToCostAttr}},
  \code{\link{getAllUserIds}}, \code{\link{getAttrValues}},
  \code{\link{getDefaultRefps}},
  \code{\link{getRoundsById}}, \code{\link{get_attrs_ID}},
  \code{\link{normalize}}
}

