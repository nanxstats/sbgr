#' wrapper of http logic for SBG API
#'
#' wrapper of http logic for SBG API
#'
#' Used for advanced users and the core method for higher level API in
#' this package, please refer to the easy api manual and the two
#' vignettes pages for more convenient usage.
#'
#' @param auth_token authenticate token string.
#' @param version API version number, default 1.1.
#' @param path path connected with base_url.
#' @param method one of 'GET', 'POST', 'PUT', 'Delete'
#' @param query Passed to httr package GET/POST call.
#' @param body Passed to httr package GET/POST/PUT/DELETE call.
#' @param base_url defeault is 'https://api.sbgenomics.com/1.1'
#'
#' @return returned request list of httr
#'
#' @references
#' \url{https://docs.sbgenomics.com/display/developerhub/API}
#'
#' @export sbgapi
#' @examples
#' token <- "fake_token"
#' \donttest{
#' ## list projects
#' sbgapi(auth_token = token, path = 'project', method = "GET")
#' }
sbgapi = function (auth_token = NULL, version = '1.1', path,
    method = c('GET', 'POST', 'PUT', 'DELETE'),
    query = NULL, body = list(),
    base_url = paste0("https://api.sbgenomics.com/", version, "/")) {

    if (is.null(auth_token))
        stop('auth_token must be provided')

    method <- match.arg(method)

    headers = c(
        'X-SBG-Auth-Token' = auth_token,
        'Accept' = 'application/json',
        'Content-type' = 'application/json'
    )

    switch(method,
           GET = {
               GET(paste0(base_url, path),
                   add_headers(.headers = headers), query = query)
           },
           POST = {
               stopifnot(is.list(body))
               body_json = toJSON(body, auto_unbox = TRUE)
               POST(paste0(base_url, path),
                    add_headers(.headers = headers), query = query,
                    body = body_json)
           },
           PUT = {
               stopifnot(is.list(body))
               body_json = toJSON(body, auto_unbox = TRUE)
               PUT(paste0(base_url, path),
                   add_headers(.headers = headers), body = body_json)
           },
           DELETE = {
               DELETE(paste0(base_url, path),
                      add_headers(.headers = headers))
           })
}


#' check request status
#'
#' check request status
#'
#' @return request content or the message
#'
#' @keywords internal
status_check = function (req) {

    if (status_code(req) %in% c('200', '201', '204')) {
        res <- content(req, "parsed")
        if(!is.null(res))
            attr(res, "response") <- req
        return(res)
    } else if (status_code(req) %in% c('401', '403', '404', '503')) {
        msg = content(req, 'parsed')$message
        stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)
    } else {
        stop('Error of unknown type occured')
    }

}
