## Upload and downloading files
##
## by Artem Sokolov

#' Instantiate a Synapse downloader
#'
#' Function creates and returns a downloader for files on Synapse.
#'
#' A downloader is itself a function that accepts a vector or list of character strings.
#' Any recognized Synapse IDs get downloaded to the local folder specified by dloc.
#' The downloader then returns local paths to these files. Strings not recognized to be
#' valid Synapse IDs are returned as is.
#'
#' Download specific versions of files stored on Synapse by suffixing the ID with
#' a version number, e.g. syn123.5
#'
#' @param dloc Path to a local folder where downloaded files will be stored
#' @param ... Additional arguments to be passed to synapser::synGet()
#' @param .cache Save files into directories inside dloc corresponding to their
#'   md5 hashes. Repeated requests for identical files will re-use already
#'   downloaded files
#' @return A downloader function that recognizes synapse IDs and downloads the associated files.
#' @examples
#' \dontrun{
#' syn <- synDownloader( "/data", ifcollision="overwrite.local" )
#' syn( "syn15663039", "localFile.csv" )
#' # [1] "/data/mtcars.csv" "localFile.csv"
#' }
#' @importFrom magrittr %>%
#' @export
synDownloader <- function( dloc, ..., .cache = FALSE )
{
    ## Define a downloader for a single id
    dl <- function( id ) {
        id_split <- strsplit(id, ".", fixed = TRUE)[[1]]
        if ( length(id_split) == 2 ) {
            id <- id_split[1]
            version <- id_split[2]
        } else
            version <- NULL
        if ( .cache ) {
          syn_meta <- synapser::synGet( id, downloadFile = FALSE, version = version, ... )
          # File name on Synapse and "download as" file name may be different
          # in rare cases. E.g. https://www.synapse.org/#!Synapse:syn21088596
          # Displayed as ROSMAP_assay_RNAseq_metadata.csv, downloaded as ROSMAP_assay_rnaSeq_metadata.csv
          # Figure out "download as" file name using direct API request
          file_handle <- synapser::synRestPOST(
            "/fileHandle/batch",
            jsonlite::toJSON(
              list(
                includeFileHandles = jsonlite::unbox(TRUE),
                includePreSignedURLs = jsonlite::unbox(FALSE),
                requestedFiles = list(
                  list(
                    fileHandleId = jsonlite::unbox(syn_meta$properties$dataFileHandleId),
                    associateObjectId = jsonlite::unbox(id),
                    associateObjectType = jsonlite::unbox("FileEntity")
                  )
                )
              )
            ),
            "https://file-prod.prod.sagebase.org/file/v1"
          )
          if ( !is.null(file_handle$requestedFiles[[1]]$failureCode) )
            stop( "Failed to obtain file handle. Error code: ", file_handle$requestedFiles[[1]]$failureCode )
          file_loc <- file.path(
            dloc, syn_meta$get("md5"), file_handle$requestedFiles[[1]]$fileHandle$fileName
          )
          if ( file.exists(file_loc) ) {
            message( "Using cached ", file_handle$requestedFiles[[1]]$fileHandle$fileName )
            return( file_loc )
          } else
            return( synapser::synGet( id, downloadLocation = file.path(dloc, syn_meta$get("md5")), version = version, ifcollision = "overwrite.local", ... )$path )
        }
        synapser::synGet( id, downloadLocation = dloc, version = version, ... )$path
    }
    ## Apply it to all synapse IDs
    function( ... )
    { purrr::flatten(list(...)) %>% purrr::map_if( isSynID(., .with_version = TRUE), dl ) %>% purrr::flatten_chr() }
}

#' Upload a directory structure to Synapse
#'
#' Recursively uploads a directory and all of its content to the given Synapse project/folder
#'
#' @param localPath A character string or vector of strings to the local file / directory to upload
#' @param parentId A synapse ID of the hosting project or parent folder on Synapse
#' @param ... Additional parameters for \link[synapser]{synStore}
#' @importFrom magrittr %>%
#' @return A nested list capturing the created hierarchy of Synapse IDs
#' @export
synStoreMany <- function( localPath, parentId, ... )
{
    if ( length(localPath) > 1 ) return( purrr::map( localPath, synStoreMany, parentId, ... ) )

    ## Case 1: localPath is a directory
    if( dir.exists(localPath) )
    {
        ## Special case: current directory
        if( localPath == "." ) return(synStoreMany( getwd(), parentId, ... ))

        ## Special case: parent directory
        if( localPath == ".." ) return(synStoreMany( dirname(getwd()), parentId, ... ))

        ## Replicate a folder to Synapse
        cat( "Creating", basename(localPath), "\n" )
        f <- synapser::Folder( name = basename(localPath), parentId = parentId ) %>% synapser::synStore()
        pid <- f$properties$id

        ## Recurse onto files in the directory
        res <- list.files( localPath, include.dirs=TRUE, full.names=TRUE ) %>%
            purrr::map( synStoreMany, pid, ... ) %>% purrr::flatten() %>% list() %>% setNames( pid )
    }

    ## Case 2: localPath is a file; simply upload it
    else
    {
        f <- synapser::File( path=localPath, parent=parentId ) %>% synapser::synStore(...)
        cat( "\n" )
        res <- list(localPath) %>% setNames( f$properties$id )
    }

    invisible(res)
}
