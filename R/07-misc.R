# 7. Misc

#' Opens browser to copy the auth token
#'
#' Click 'Generate Token' button, copy and paste the generated token
#' string to the R console. The function will return the token string.
#'
#' @return auth token
#'
#' @export misc_get_auth_token
#' @importFrom utils browseURL
#' @examples
#' # Paste the auth token into R
#' # console then press enter:
#' token = NULL
#' \donttest{token = misc_get_auth_token()}
misc_get_auth_token = function () {

    browseURL('https://igor.sbgenomics.com/account/?current=developer#developer')
    cat("\nEnter the generated authentication token:")
    auth_token = scan(what = character(), nlines = 1L, quiet = TRUE)

    return(auth_token)

}

#' Download SBG uploader and extract to a specified directory
#'
#' Download SBG uploader and extract to a specified directory.
#'
#' @return \code{0L} if the SBG CLI uploader is successfully
#' downloaded and unarchived.
#'
#' @param destdir The directory to extract SBG uploader to.
#' If not present, it will be created automatically.
#'
#' @export misc_get_uploader
#' @importFrom utils untar download.file
#' @examples
#' dir = '~/sbg-uploader/'
#' \donttest{misc_get_uploader(dir)}
misc_get_uploader = function (destdir = NULL) {

    if (is.null(destdir)) stop('destdir must be provided')

    tmpfile = tempfile()

    download.file(url = 'https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz',
                  method = 'libcurl', destfile = tmpfile)

    untar(tarfile = tmpfile, exdir = path.expand(destdir))

}

#' Specify the parameters of the file metadata and return a list,
#' JSON string, or write to a file
#'
#' Specify the parameters of the file metadata and return a list,
#' JSON string, or write to a file.
#'
#' For more information about file metadata, please check the
#' File Metadata Documentation:
#' \url{https://developer.sbgenomics.com/platform/metadata}.
#'
#' @param output Output format,
#' could be \code{'list'}, \code{'json'}, or \code{'metafile'}.
#' @param destfile Filename to write to.
#' Must be specified when \code{output = 'metafile'}.
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
#' parameter is only required by some the tools and pipelines; however,
#' it is strongly recommended that you set it whenever possible, unless
#' you are certain that your pipeline will work without it.
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
#'
#' @return list, JSON string, or a file.
#'
#' @export misc_make_metadata
#'
#' @references
#' \url{https://developer.sbgenomics.com/platform/metadata}
#'
#' @examples
#' destfile = '~/c.elegans_chr2_test.fastq.meta'
#' \donttest{misc_make_metadata(output = 'metafile',
#'             destfile = destfile,
#'             name = 'c.elegans_chr2_test.fastq',
#'             file_type = 'fastq', qual_scale = 'illumina13',
#'             seq_tech = 'Illumina')}
misc_make_metadata = function (output = c('list', 'json', 'metafile'),
                               destfile = NULL,
                               name = NULL,
                               file_type = c('text', 'binary', 'fasta',
                                             'csfasta', 'fastq', 'qual',
                                             'xsq', 'sff', 'bam', 'bam_index',
                                             'illumina_export', 'vcf', 'sam',
                                             'bed', 'archive', 'juncs',
                                             'gtf','gff', 'enlis_genome'),
                               qual_scale = c('sanger', 'illumina13',
                                              'illumina15', 'illumina18',
                                              'solexa'),
                               seq_tech = c('454', 'Helicos', 'Illumina',
                                            'Solid', 'IonTorrent'),
                               sample = NULL, library = NULL,
                               platform_unit = NULL, paired_end = NULL) {

    body = list(list('file_type' = file_type,
                     'qual_scale' = qual_scale,
                     'seq_tech' = seq_tech))
    names(body) = 'metadata'

    if (!is.null(sample)) body$'metadata'$'sample' = as.character(sample)
    if (!is.null(library)) body$'metadata'$'library' = as.character(library)
    if (!is.null(platform_unit)) body$'metadata'$'platform_unit' = as.character(platform_unit)
    if (!is.null(paired_end)) body$'metadata'$'paired_end' = as.character(paired_end)

    if (!is.null(name)) body = c(list('name' = name), body)

    if (output == 'metafile') {
        if (is.null(destfile)) stop('destfile must be provided')
        body = toJSON(body, auto_unbox = TRUE)
        writeLines(body, con = destfile)
    } else if (output == 'json') {
        body = toJSON(body, auto_unbox = TRUE)
        return(body)
    } else if (output == 'list') {
        return(body)
    }

}

#' Upload files using SBG uploader
#'
#' Upload files using SBG uploader.
#'
#' @return The uploaded file's ID number.
#'
#' @param auth_token auth token
#' @param uploader The directory where the SBG uploader is located
#' (the directory that contains the bin/ directory).
#' @param file The location of the file to upload.
#' @param project_id The project ID to upload the files to.
#' If you do not supply this, then the uploader will place the
#' incoming files in your "My Files" section.
#' @param proxy Allows you to specify a proxy server through which
#' the uploader should connect. About the details the proxy parameter format,
#' see \url{https://developer.sbgenomics.com/tools/uploader/documentation}.
#'
#' @export misc_upload_cli
#'
#' @references
#' \url{https://developer.sbgenomics.com/tools/uploader/documentation}
#'
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{misc_upload_cli(auth_token = token,
#'                           uploader = '~/sbg-uploader/',
#'                           file = '~/example.fastq', project_id = '1234')}
misc_upload_cli = function (auth_token = NULL, uploader = NULL,
                            file = NULL, project_id = NULL,
                            proxy = NULL) {

    if (is.null(auth_token)) stop('auth_token must be provided')
    if (is.null(uploader)) stop('SBG uploader location must be provided')
    if (is.null(file)) stop('File location must be provided')

    auth_token = paste('-t', auth_token)
    uploader = file.path(paste0(uploader, '/bin/sbg-uploader.sh'))
    file = file.path(file)

    if (!is.null(project_id)) project_id = paste('-p', project_id)
    if (!is.null(proxy)) proxy = paste('-x', proxy)

    cmd = paste(uploader, auth_token, project_id, proxy, file)
    res = system(command = cmd, intern = TRUE)
    fid = strsplit(res, '\t')[[1]][1]
    return(fid)

}

## 99.easy_api.R MISC
response <- function(x){
    attr(x, "response")
}

.getFields <- function(x, values) {
    ## from Martin's code
    flds = names(x$getRefClass()$fields())
    if (!missing(values))
        flds = flds[flds %in% values]
    result = setNames(vector("list", length(flds)), flds)
    for (fld in flds)
        result[[fld]] = x[[fld]]
    result
}

stopifnot_provided <- function(..., msg = "is not provided"){
    n <- length(ll <- list(...))
    if(n == 0)
        return(invisible())
    mc <- match.call()
    x = NULL
    for(i in 1:n){
        if(!(is.logical(r <- eval(ll[[i]])) && all(r))){
            l <- mc[[i+1]][[2]]
            x <- c(x, deparse(l[[length(l)]]))
        }
    }
    if(length(x))
        stop(paste(paste(x, collapse = ","), msg), call. = FALSE)
}




m.fun <- function(x, y, exact = TRUE, ignore.case = TRUE, ...){
    if(exact){
        pmatch(x, y, ...)
    }else{
        grep(x, y, ignore.case = ignore.case, ...)
    }
}

## match by id and name
m.match <- function(obj, id = NULL, name = NULL,
                    .id = "id",
                    .name = "name",
                    exact = TRUE, ignore.case = TRUE){
    ## if no match, return whole list
    if(is.null(id)){
        if(is.null(name)){
            return(obj)
        }else{
            ## id is null, so use username
            nms <- sapply(obj, function(x) x[[.name]])
            if(ignore.case){
                name <- tolower(name)
                nms <- tolower(nms)
            }
            index <- m.fun(name, nms,
                           exact = exact,
                           ignore.case = ignore.case)
        }
    }else{
        ## id is not NULL
        ids <- sapply(obj, function(x) x[[.id]])
        index <- m.fun(id, ids,
                       exact = exact,
                       ignore.case = ignore.case)

    }
    if(length(index) == 1 && is.na(index)){
        message("sorry, no matching ")
        return(NULL)
    }else{
        if(length(index) ==1){
            obj[[index]]
        }else{
            obj[index]
        }
    }
}


.showFields <- function(x, title = NULL, values = NULL){
    if (missing(values)){
        flds = names(x$getRefClass()$fields())
    }else{
        flds = values
    }

    ## if(is.null(title))
    ##     title <- class(x)
    if(!is.null(title)){
        message(title)
    }

    for (fld in flds)
        message(fld, " : ", x[[fld]])

}

.showList <- function(x){
    if(length(x)){
        x <- x[!sapply(x, is.null)]
        for (fld in names(x))
            message(fld, " : ", x[[fld]])
    }
}


.update_list <- function(o, n){
    o.nm <- names(o)
    n.nm <- names(n)
    i.nm <- intersect(o.nm, n.nm)

    if(length(i.nm)){
        o.nm <- setdiff(o.nm, i.nm)
        c(o[o.nm], n)
    }else{
        c(o, n)
    }
}


