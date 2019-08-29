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
#' @param dloc Path to a local folder where downloaded files will be stored
#' @param useCache Use locally cached file if file has already been download and MD5 hasn't changed
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
synDownloader <- function( dloc, useCache=FALSE, ... )
{
    ## Define a downloader for a single id
    dl <- function( id )
    {
        remoteFile <- synapser::synGet( id, downloadFile=FALSE )
        localPath <- file.path( dloc, remoteFile$properties$name )
        if ( useCache && file.exists( localPath ) )
        {
            message( "Cached version of ", remoteFile$properties$name, " found. Checking MD5 match." )
            if ( remoteFile$get( "md5" ) == tools::md5sum( localPath ) )
            {
                message( "MD5 identical, using cached file." )
                return( localPath )
            }
            message( "MD5 doesn't match, download from Synapse." )
        }
        synapser::synGet( id, downloadLocation = dloc, ... )$path
    }

    ## Apply it to all synapse IDs
    function( ... )
    { purrr::flatten(list(...)) %>% purrr::map_if( isSynID, dl ) %>% purrr::flatten_chr() }
}

#' Upload a directory structure to Synapse
#'
#' Recursively uploads a directory and all of its content to the given Synapse project/folder
#'
#' @param localPath A character string to the local file / directory to upload
#' @param parentId A synapse ID of the hosting project or parent folder on Synapse
#' @importFrom magrittr %>%
#' @return A nested list capturing the created hierarchy of Synapse IDs
#' @export
synStoreMany <- function( localPath, parentId )
{
    ## Case 1: localPath is a directory
    if( dir.exists(localPath) )
    {
        ## Special case: current directory
        if( localPath == "." ) return(synStoreMany( getwd(), parentId ))

        ## Special case: parent directory
        if( localPath == ".." ) return(synStoreMany( dirname(getwd()), parentId ))

        ## Replicate a folder to Synapse
        cat( "Creating", basename(localPath), "\n" )
        f <- synapser::Folder( name = basename(localPath), parentId = parentId ) %>% synapser::synStore()
        pid <- f$properties$id

        ## Recurse onto files in the directory
        res <- list.files( localPath, include.dirs=TRUE, full.names=TRUE ) %>%
            purrr::map( synStoreMany, pid ) %>% purrr::flatten() %>% list() %>% setNames( pid )
    }

    ## Case 2: localPath is a file; simply upload it
    else
    {
        f <- synapser::File( path=localPath, parent=parentId ) %>% synapser::synStore()
        cat( "\n" )
        res <- list(localPath) %>% setNames( f$properties$id )
    }

    invisible(res)
}
