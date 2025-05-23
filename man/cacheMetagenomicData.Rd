% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMetagenomicData.R
\name{cacheMetagenomicData}
\alias{cacheMetagenomicData}
\title{Retrieve and cache MetaPhlAn output files}
\usage{
cacheMetagenomicData(
  uuids,
  data_type = "relative_abundance",
  ask_on_update = TRUE
)
}
\arguments{
\item{uuids}{Vector of strings: sample UUID(s) to get output for}

\item{data_type}{Single string: 'relative_abundance' or 'viral_clusters', indicating
which output files to get, Default: 'relative_abundance'}

\item{ask_on_update}{Boolean: should the function ask the user before
re-downloading a file that is already present in the cache, Default: TRUE}
}
\value{
A tibble with information on the cached files, including UUID, data
type, Google Cloud Bucket object name, local cache ID, and cached file path
}
\description{
'cacheMetagenomicData' takes UUID and data type arguments,
downloads the corresponding MetaPhlAn output files, and stores them in a
local parkinsonsMetagenomicData cache. If the same files are requested
again through this function, they will not be re-downloaded unless explicitly
specified, in order to reduce excessive downloads.
}
\examples{
\dontrun{
if(interactive()){
 cacheMetagenomicData(uuid = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
                      data_type = "relative_abundance")
 }
}
}
\seealso{
\code{\link[stringr]{str_split}}
\code{\link[tibble]{tibble}}
}
