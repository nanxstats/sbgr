# 6. Tasks

#' Returns the list of all the tasks for a project
#'
#' Returns the list of all the tasks for a project.
#'
#' This function returns general information and status of a task,
#' in case you want to get a details, including the inputs, outputs
#' and parameters set for that task, you will have to use task details
#' resource referencing the \code{task_id} of a task that you want to
#' get information about.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export task_list
#'
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = task_list(token,
#'                 '1c1d06d2-5862-48f6-b595-e0099b20937e')}
task_list = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be both provided')

    req = sbgapi(auth_token = auth_token,
        path = paste0('project/', project_id, '/task'),
        method = 'GET', ...)

    return(status_check(req))

}

#' Runs a task as a part of a project
#'
#' Runs a task as a part of a project.
#'
#' All the details, including the pipeline ID and runtime parameters,
#' are specified via a list. See the example for details.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param task_details A list with the following components:
#' \itemize{
#' \item \code{pipeline_id} - ID of the pipeline you wish to execute
#' \item \code{pipeline_revision} - Revision of the pipeline you wish
#' to execute. If not specified, latest revision is used.
#' \item \code{name} - Name of the task you wish to execute
#' \item \code{description} - Description of the task you wish to execute
#' \item \code{inputs} - Named list containing mappings of pipeline input
#' node ID to file IDs. Note that file IDs always need to be specified as
#' an list, even if empty or with one element.
#' \item \code{parameters} - Named list containing mappings of node IDs
#' to apps specific parameters. Note that parameters are always specified
#' as an list, even if empty or with one element.}
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export task_run
#'
#'
#' @examples
#' token = '58aeb140-1970-0130-6386-001f5b34aa78'
#' details = list(
#'   'name' = 'Test 2 of C. Elegans VC',
#'   'description' = 'Testing Caenorhabditis elegans Exome Variant Calling',
#'   'pipeline_id' = '422',
#'   'inputs' = list('309485' = 13645,
#'                   '317344' = 13646,
#'                   '318662' = 13645,
#'                   '699018' = 13647),
#'   'parameters' = list('393463' = list('read_trimming_qual' = 30,
#'                                       'rg_seq_tech' = 'Illumina'),
#'                       '677492' = list()))
#'
#' \donttest{req = task_run(token,
#'                 project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'                 task_details = details)}
task_run = function (auth_token = NULL,
                     project_id = NULL, task_details = NULL, ...) {

    if (is.null(project_id) || is.null(task_details))
        stop('project_id and task_details must be both provided')

    body = task_details

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/task'),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Returns information about the task
#'
#' Returns information about the task.
#'
#' Each task has a status and status message, containing the more detailed
#' information about the task status, associated with it. This is a list of
#' all values that task status can have:
#' \itemize{
#' \item \code{active} - Task is currently running.
#' \item \code{completed} - Task has finished successfully.
#' \item \code{aborted} - Task was aborted by user.
#' \item \code{failed} - Task has failed to finish due to
#' either bad inputs and/or parameters, or because of the
#' internal infrastructure failures.}
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param task_id ID of a task you want to access.
#' @param download.url Logical. Return the download URL or not.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export task_details
#'
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req1 = task_details(token,
#'                  project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'                  task_id = '22237')
#' req2 = task_details(token,
#'        project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'        task_id = '22237', download.url = TRUE)}
task_details = function (auth_token = NULL,
                         project_id = NULL, task_id = NULL,
                         download.url = FALSE, ...) {

    if (is.null(project_id) || is.null(task_id))
        stop('project_id and task_id must be both provided')

    if (download.url == FALSE) {
        req = sbgapi(auth_token = auth_token,
                     path = paste0('project/', project_id, '/task/', task_id),
                     method = 'GET', ...)
    } else {
        req = sbgapi(auth_token = auth_token,
                     path = paste0('project/', project_id, '/task/', task_id),
                     query = list('action' = 'download'), method = 'GET', ...)
    }

    return(status_check(req))

}

#' Performs action on the task
#'
#' Performs action on the task.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param task_id ID of a task you want to access.
#' @param action Character string specifying the action.
#' Currently, only supported action is \code{'abort'}.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export task_action
#'
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = task_action(token,
#'                 project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'                 task_id = '5506a44ae4b04a4ab3ae7250',
#'                 action = 'abort')}
task_action = function (auth_token = NULL, project_id = NULL,
                        task_id = NULL, action = 'abort', ...) {

    if (is.null(project_id) || is.null(task_id))
        stop('project_id and task_id must be both provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/task/', task_id),
                 query = list('action' = action), method = 'POST', ...)

    return(status_check(req))

}
