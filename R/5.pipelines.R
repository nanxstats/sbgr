# 5. Pipelines

#' Returns the list of all public pipelines
#'
#' Returns the list of all public pipelines.
#'
#' @param auth_token auth token
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_list_pub
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/get/pipeline/public}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = pipeline_list_pub(token)}
pipeline_list_pub = function (auth_token = NULL, ...) {

    req = sbgapi(auth_token = auth_token,
                 path = 'pipeline/public', method = 'GET', ...)

    return(status_check(req))

}

#' Returns the list of pipelines in user's "My Pipelines" section
#'
#' Returns the list of pipelines in user's "My Pipelines" section.
#'
#' @param auth_token auth token
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_list_my
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/get/pipeline/my}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = pipeline_list_my(token)}
pipeline_list_my = function (auth_token = NULL, ...) {

    req = sbgapi(auth_token = auth_token, path = 'pipeline/my', method = 'GET', ...)

    return(status_check(req))

}

#' Returns a list of all the pipelines in project
#'
#' Returns a list of all the pipelines in project.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_list_project
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/get/project/\%3Aproject_id/pipeline}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = pipeline_list_project(token,
#'             project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858')}
pipeline_list_project = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/pipeline'),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Returns the details of a pipeline for a project
#'
#' Returns the details of a pipeline (runtime and regular parameters,
#' description etc.) for a project.
#'
#' When using the API to run a task, the user needs to set input files
#' for all input nodes. To facilitate this, some pipeline input nodes
#' may contain field "suggested files", that contains files which may
#' be used as default input (reference genomes, SNP database, etc.).
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param pipeline_id ID of a pipeline you want to access.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_details
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/get/project/\%3Aproject_id/pipeline/\%3Apipeline_id}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = pipeline_details(token,
#'             project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858',
#'             pipeline_id = '55606ad4896a5d524656afd0')}
pipeline_details = function (auth_token = NULL,
                             project_id = NULL, pipeline_id = NULL, ...) {

    if (is.null(project_id) || is.null(pipeline_id))
        stop('project_id and pipeline_id must be both provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id,
                               '/pipeline/', pipeline_id),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Add a pipeline to a specified project
#'
#' Add a pipeline to a specified project. You can use this function to add
#' a pipeline from your other project or a public pipeline to a project.
#'
#' @param auth_token auth token
#' @param project_id_to ID of a project you to copy pipeline into.
#' @param project_id_from ID of the project you wish to add from.
#' Specify values such as \code{"my"} to specify a pipeline from
#' "My Pipelines" section or omit for a public pipeline, respectively.
#' @param pipeline_id ID of the pipeline you wish to add to project.
#' @param revision Revision of the pipeline you wish to add to the project.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_add
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/post/project/\%3Aproject_id/pipeline}
#'
#' @examples
#' token = '58aeb140-1970-0130-6386-001f5b34aa78'
#' \donttest{req = pipeline_add(token,
#'             project_id_to = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'             project_id_from = 'f0eb447f-3511-4b28-9253-eba96191d432',
#'             pipeline_id = '53452130d79f0049c0c94441')}
pipeline_add = function (auth_token = NULL, project_id_to = NULL,
                         project_id_from = NULL, pipeline_id = NULL,
                         revision = NULL, ...) {

    if (is.null(project_id_to) || is.null(pipeline_id))
        stop('project_id_to and pipeline_id must be provided')

    if (is.null(project_id_from)) {
        body = list('pipeline_id' = pipeline_id)
    } else {
        body = list('project_id' = project_id_from,
                    'pipeline_id' = pipeline_id)
    }

    if (!is.null(revision)) body = c(body, 'revision' = as.character(revision))

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id_to, '/pipeline'),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}
