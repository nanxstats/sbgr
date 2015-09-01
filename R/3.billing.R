# 3. Billing

#' Returns the list of all billing groups you have access to
#'
#' Returns the list of all billing groups you have access to.
#' This is an utility method used only create projects with
#' appropriate billing group. Full access to billing data is not
#' available via the SBG public API yet.
#'
#' @param auth_token auth token
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export billing
#'
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = billing(token)}
billing = function (auth_token = NULL, ...) {

    req = sbgapi(auth_token = auth_token, path = 'billing', method = 'GET', ...)

    return(status_check(req))

}
