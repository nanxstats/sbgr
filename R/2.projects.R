# 2. Projects

#' Returns the list of all projects you have access to
#'
#' Returns the list of all projects you have access to.
#'
#' @param auth_token auth token
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export project_list
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/get/project}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_list(token)}
project_list = function (auth_token = NULL, ...) {

    req = sbgapi(auth_token = auth_token, path = 'project', method = 'GET', ...)

    return(status_check(req))

}

#' Returns the details of the project
#'
#' Returns the details of the project.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export project_details
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/get/project/\%3Aproject_id}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_details(token,
#'                 project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858')}
project_details = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id), method = 'GET', ...)

    return(status_check(req))

}

#' Returns a list of all users invited to the project and their privileges
#'
#' Returns a list of all users invited to the project and their privileges.
#' Project ID is specified as path parameter. Call returns ID and username
#' of the user with privileges.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export project_members
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/get/project/\%3Aproject_id/members}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_members(token,
#'                 project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858')}
project_members = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/members'),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Create new project
#'
#' You can use this call to create a project. All details, including
#' project name, description and funding source are specified as part
#' of the JSON, sent as the body of the request. This call returns
#' details of the project.
#'
#' @param auth_token auth token
#' @param name Name of the project you wish to create.
#' @param description Description of the project you wish to create.
#' @param billing_group_id ID of the billing group you wish to use
#' for this project.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export project_new
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/post/project}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_new(token, name = 'Test API project',
#'                 description = 'My first API project',
#'                 billing_group_id = '5b6d5e71-dff8-42fc-8583-500d858f1093')}
project_new = function (auth_token = NULL, name = NULL,
                        description = NULL, billing_group_id = NULL, ...) {

    if (is.null(name) || is.null(description) || is.null(billing_group_id))
        stop('name, description, and billing_group_id must be provided')

    body = list('name' = name,
                'description' = description,
                'billing_group_id' = billing_group_id)

    req = sbgapi(auth_token = auth_token,
                 path = 'project', body = body,
                 method = 'POST', ...)

    return(status_check(req))

}

#' Add a user to the project with appropriate permissions
#'
#' You can use this call to add specific users to a project and set their
#' privileges. Note that you need to specify user's SBG platform username
#' when adding to the project.
#'
#' @param auth_token auth token
#' @param project_id Name of the project you wish to add user to.
#' @param username SBG platform username for a user you wish to add to
#' the project.
#' @param copy Logical. Ability to download or copy files.
#' @param write Logical. Ability to create/edit/delete project objects.
#' @param execute Logical. Ability to run tasks.
#' @param admin Logical. User has all rights on the project
#' (including changing).
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export project_member_add
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/post/project/\%3Aproject_id/members}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_member_add(token,
#'                 project_id = '88fc89c1-cfcd-46ed-a830-6a2fc110c628',
#'                 username = 'testuser', write = TRUE)}
project_member_add = function (auth_token = NULL, project_id = NULL,
                               username = NULL, copy = FALSE, write = FALSE,
                               execute = FALSE, admin = FALSE, ...) {

    if (is.null(project_id) || is.null(username))
        stop('project_id and username must be both provided')

    body = list('username' = username,
                'permissions' = list(
                    'copy' = copy, 'write' = write,
                    'execute' = execute, 'admin' = admin))

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/members'),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Set permissions for a user to a project
#'
#' This call will set project's member privileges.
#' Privileges you do not explicitly set to "true" will be automatically
#' set to "false". Project ID and user ID are specified in path parameters.
#' Note that you must get the user IDs by performing the project_members()
#' call and gathering id of the user with a specific permission.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param user_id ID of a user whose permissions you with to set
#' @param write Logical. Ability to create/edit/delete project objects.
#' @param copy Logical. Ability to download or copy files.
#' @param execute Logical. Ability to run tasks.
#' @param admin Logical. User has all rights on the project
#' (including changing).
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export project_member_update
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/put/project/\%3Aproject_id/members/\%3Auser_id}
#'
#' @examples
#' token = '58aeb140-1970-0130-6386-001f5b34aa78'
#' \donttest{req = project_member_update(token,
#'             project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'             user_id = '08890148-6d9e-4a10-b284-924228d3f99a')}
project_member_update = function (auth_token = NULL,
                                  project_id = NULL, user_id = NULL,
                                  write = FALSE, copy = FALSE,
                                  execute = FALSE, admin = FALSE, ...) {

    if (is.null(project_id) || is.null(user_id))
        stop('project_id and user_id must be both provided')

    body = list('write' = write,
                'copy' = copy,
                'execute' = execute,
                'admin' = admin)

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/members/', user_id),
                 body = body, method = 'PUT', ...)

    return(status_check(req))

}

#' Delete a project
#'
#' Note that this deletes all files, tasks which belong to a project.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to delete.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export project_delete
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/delete/project/\%3Aproject_id}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_delete(token,
#'             project_id = '3a21ade8-ef3e-41f8-8ac2-1dc3b434ac77')}
project_delete = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id), method = 'DELETE', ...)

    return(status_check(req))

}

#' Removes a member from a project
#'
#' Note that user_id parameter is not username, but user ID parameter
#' that you can receive from GET members call.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param user_id ID of the user you wish to remove.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export project_member_delete
#'
#' @references
#' \url{https://developer.sbgenomics.com/api/1.1/delete/project/\%3Aproject_id/members/\%3Auser_id}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_member_delete(token,
#'             project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'             user_id = '08890148-6d9e-4a10-b284-924228d3f99a')}
project_member_delete = function (auth_token = NULL,
                                  project_id = NULL, user_id = NULL, ...) {

    if (is.null(project_id) || is.null(user_id))
        stop('project_id and user_id must be both provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/members/', user_id),
                 method = 'DELETE', ...)

    return(status_check(req))

}
