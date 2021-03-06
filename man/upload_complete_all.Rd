% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-upload.R
\name{upload_complete_all}
\alias{upload_complete_all}
\title{Reports the complete file upload}
\usage{
upload_complete_all(auth_token = NULL, upload_id = NULL, ...)
}
\arguments{
\item{auth_token}{auth token}

\item{upload_id}{ID of the upload}

\item{...}{parameters passed to sbgapi function}
}
\value{
parsed list of the returned json
}
\description{
If the whole parts are uploaded, and the provided ETags are correct,
then the file is assembled and made available on the SBG platform.
}
\examples{
token = '58aeb140-1970-0130-6386-001f5b34aa78'
\donttest{req = upload_complete_all(token,
            upload_id = '8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT')}
}
