## Additional functionality for synapser
##
## by Artem Sokolov

#' Check validity of Synapse IDs
#' 
#' Identifies if its argument is a valid Synapse ID character string
#'
#' @param ids A vector or list containing one or more objects to identify.
#' @return Logical values indicating whether each element of the input is a valid Synapse ID.
#' @examples
#' isSynID( c("syn1234", "syn", "syn123ab" ) )
#' # [1]  TRUE FALSE FALSE
#' isSynID( list( mtcars, 123, "syn123" ) )
#' # [1] FALSE FALSE  TRUE
#' @export
isSynID <- function( ids )
{ purrr::map_lgl( ids, is.character ) & grepl( "^syn[0-9]+$", ids ) }

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
#' @param ... Additional arguments to be passed to synapser::synGet()
#' @return A downloader function that recognizes synapse IDs and downloads the associated files.
#' @examples
#' syn <- synDownloader( "/data", ifcollision="overwrite.local" )
#' syn( "syn15663039", "localFile.csv" )
#' # [1] "/data/mtcars.csv" "localFile.csv"
#' @importFrom magrittr %>%
#' @export
synDownloader <- function( dloc, ... )
{
    ## Define a downloader for a single id
    dl <- function( id )
    { synapser::synGet( id, downloadLocation = dloc, ... )$path }

    ## Apply it to all synapse IDs
    function( ... )
    { purrr::flatten(list(...)) %>% purrr::map_if( isSynID, dl ) %>% purrr::flatten_chr() }
}

