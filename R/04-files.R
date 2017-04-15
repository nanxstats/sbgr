# # 4. Files

#' Returns the list of all project files for a project
#'
#' Returns the list of all project files for a project. If user specifies
#' string \code{"public"} as \code{project_id}, this will return a list
#' of public files.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' Note that specifying \code{"public"} you can list public files.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export file_list
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_list(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e')}
file_list = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file'), method = 'GET', ...)

    return(status_check(req))

}

#' Returns detailed information about a project's files
#'
#' Returns detailed information about a project's files
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param file_id ID of a file you want to access.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export file_details
#'
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_details(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'             file_id = '530854e2e4b036506b803c7e')}
file_details = function (auth_token = NULL, project_id = NULL,
    file_id = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file/', file_id),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Copy specified file(s) to the specified project
#'
#' Copy specified file(s) to the specified project
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to copy files to.
#' @param file_id Character vector. IDs of the files you wish to copy to
#' the project.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export file_copy
#'
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_copy(token,
#'             project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'             file_id = c('5506a44ae4b04a4ab3ae7250',
#'                         '5506a44ae4b04a4ab3ae7254',
#'                         '5506a44ae4b04a4ab3ae7252'))}
file_copy = function (auth_token = NULL, project_id = NULL,
    file_id = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    body = list('file_id' = as.character(file_id))

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file'),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Update project's file metadata
#'
#' This function updates project's file metadata. You can also use this call
#' to change filenames if you supply the \code{name} argument.
#'
#' For more information about file metadata, please check the
#' File Metadata Documentation:
#' \url{https://developer.sbgenomics.com/platform/metadata}.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param file_id ID of a file you want to access.
#' @param name File name.
#' @param file_type File type. This metadata parameter is mandatory
#' for each file.
#' @param qual_scale Quality scale encoding. For FASTQ files, you must
#' either specify the quality score encoding sch which contains the
#' FASTQ quality scale detector wrapper. In that case, you can
#' specify the quality score encoding scheme by setting
#' \code{qual_scale} inside the pipeline. For BAM files, this value
#' should always be \code{'sanger'}.
#' @param seq_tech Sequencing technology. The \code{seq_tech} parameter
#' allows you to specify the sequencing technology used. This metadata
#' parameter is only required by some the tools and pipelines;
#' however, it is strongly recommended that you set it whenever possible,
#' unless you are certain that your pipeline will work without it.
#' @param sample Sample ID. You can use the \code{sample} parameter to specify
#' the sample identifier. The value supplied in this field will be written
#' to the read group tag (\code{@@RG:SM}) in SAM/BAM files generated from reads
#' with the specified Sample ID. AddOrReplaceReadGroups will use this
#' parameter as the value for the read group tag in a SAM/BAM file.
#' @param library Library. You can set the library for the read using the
#' \code{library} parameter. The value supplied in this field will be written
#' to the read group tag (\code{@@RG:LB}) in SAM/BAM files generated from
#' reads with the specified Library ID. AddOrReplaceReadGroups will use
#' this parameter as the value for the read group tag in a SAM/BAM file.
#' @param platform_unit Platform unit. You can set the platform unit
#' (e.g. lane for Illumina, or slide for SOLiD) using the \code{platform_unit}
#' parameter. The value supplied in this field will be written to the read
#' group tag (\code{@@RG:PU}) in SAM/BAM files generated from the reads with
#' the specified Platform Unit. AddOrReplaceReadGroups will use this parameter
#' as the value for the read group tag of a SAM/BAM file.
#' @param paired_end Paired end. With paired-end reads, this parameter
#' indicates if the read file is left end (1) or right end (2).
#' For SOLiD CSFASTA files, paired end files 1 and 2 correspond to R3
#' and F3 files, respectively.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export file_meta_update
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_meta_update(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'             file_id = '530854e2e4b036506b803c7e',
#'             name = 'c.elegans_chr2_test.fastq',
#'             file_type = 'fastq', qual_scale = 'illumina13',
#'             seq_tech = 'Illumina')}
file_meta_update = function (auth_token = NULL,
                             project_id = NULL, file_id = NULL,
                             name = NULL,
                             file_type = c('text', 'binary', 'fasta', 'csfasta',
                                           'fastq', 'qual', 'xsq', 'sff', 'bam',
                                           'bam_index', 'illumina_export',
                                           'vcf', 'sam', 'bed', 'archive',
                                           'juncs', 'gtf','gff',
                                           'enlis_genome'),
                             qual_scale = c('sanger', 'illumina13',
                                            'illumina15', 'illumina18',
                                            'solexa'),
                             seq_tech = c('454', 'Helicos', 'Illumina', 'Solid',
                                          'IonTorrent'),
                             sample = NULL, library = NULL,
                             platform_unit = NULL, paired_end = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    body = list(list('file_type' = file_type,
                     'qual_scale' = qual_scale,
                     'seq_tech' = seq_tech))
    names(body) = 'metadata'

    if (!is.null(sample)) body$'metadata'$'sample' = as.character(sample)
    if (!is.null(library)) body$'metadata'$'library' = as.character(library)
    if (!is.null(platform_unit)) body$'metadata'$'platform_unit' = as.character(platform_unit)
    if (!is.null(paired_end)) body$'metadata'$'paired_end' = as.character(paired_end)

    if (!is.null(name)) body = c(list('name' = name), body)

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file/', file_id),
                 body = body, method = 'PUT', ...)

    return(status_check(req))

}

#' Removes a file from a project
#'
#' Removes a file from a project
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param file_id ID of a file you want to delete.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export file_delete
#'
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_delete(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'             file_id = '530854e2e4b036506b803c7e')}
file_delete = function (auth_token = NULL,
                        project_id = NULL, file_id = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file/', file_id),
                 method = 'DELETE', ...)

    return(status_check(req))

}

#' Returns a direct download URL for a project's file
#'
#' Returns a direct download URL for a project's file.
#'
#' You can use any HTTP client, or library to access or download
#' the content once you get the URL.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param file_id ID of a file you want to access.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export file_download_url
#'
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_download_url(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'             file_id = '530854e2e4b036506b803c7e')}
file_download_url = function (auth_token = NULL,
                              project_id = NULL, file_id = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id,
                               '/file/', file_id, '/download'),
                 method = 'GET', ...)

    return(status_check(req))

}
