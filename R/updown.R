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
#' @return A downloader function that recognizes synapse IDs and downloads the associated files.
#' @examples
#' \dontrun{
#' syn <- synDownloader( "/data", ifcollision="overwrite.local" )
#' syn( "syn15663039", "localFile.csv" )
#' # [1] "/data/mtcars.csv" "localFile.csv"
#' }
#' @importFrom magrittr %>%
#' @export
synDownloader <- function( dloc, ... )
{
    ## Define a downloader for a single id
    dl <- function( id ) {
        id_split <- strsplit(id, ".", fixed = TRUE)[[1]]
        if ( length(id_split) == 2 ) {
            id <- id_split[1]
            version <- id_split[2]
        } else
            version <- NULL
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
