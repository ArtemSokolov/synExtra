## Additional functionality for synapser
##
## by Artem Sokolov

#' Check validity of Synapse IDs
#' 
#' Identifies if its argument is a valid Synapse ID character string
#'
#' @param ... One or more Synapse IDs. Accepts individual IDs, vectors or lists.
#' @return Logical values indicating whether each element of the input is a valid Synapse ID.
#' @examples
#' isSynID( "syn1234", "syn", "syn123ab" )
#' # [1]  TRUE FALSE FALSE
#' isSynID( list( mtcars, 123, "syn123" ) )
#' # [1] FALSE FALSE  TRUE
#' @export
isSynID <- function( ... )
{
    ids <- purrr::flatten(list(...))
    purrr::map_lgl( ids, is.character ) & grepl("^syn[0-9]+$", ids)
}

#' Look up filename based on synapse ID
#' 
#' Retrieves file names associated with given synapse IDs
#' 
#' @param ... One or more Synapse IDs. Accepts individual IDs, vectors or lists.
#' @return A character vector of filenames corresponding to the provided synapse IDs
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' synName( "syn1896429", "syn1901530" )
#' #                            syn1896429                              syn1901530 
#' #        "ESTIMATE_scores_PANCAN11.tsv" "ESTIMATE_scores_PANCAN11_RNASeqV2.tsv" 
#' }
#' @export
synName <- function( ... )
{
    ids <- purrr::flatten( list(...) ) %>% purrr::flatten_chr()
    
    ## Isolate the unique set of ids and retrieve the name for each
    idMap <- unique(ids) %>% purrr::set_names() %>%
        purrr::map_chr( ~synapser::synGet( .x, downloadFile=FALSE )$properties$name )

    ## Extend the mapping to all the requested values
    idMap[ids]
}
    
#' Retrieve parentId chain
#'
#' Recursively reconstructs parentId ancestry for a given Synapse ID
#'
#' @param ... One or more Synapse IDs for which ancestry is to be reconstructed
#' @return For each requested ID, the function returns a vector of parentId ancestry.
#' The first entry in each vector is the query ID itself; the last entry is the project.
#' @examples
#' \dontrun{
#' synAncestry( "syn15663039", "syn1695362" )
#' # [[1]]
#' # [1] "syn15663039" "syn15673834" "syn15673837" "syn12180284"
#' #
#' # [[2]]
#' # [1] "syn1695362" "syn1695324" "syn2812925" "syn300013" 
#' }
#' @export
synAncestry <- function(...)
{
    ## Split up computation, if multiple IDs are provided
    l <- purrr::flatten(list(...)) %>% purrr::set_names()
    if( length(l) < 1 ) stop( "Please provide at least one synapse ID" )
    if( length(l) > 1 ) return( purrr::map(l, synAncestry) )
    id <- unname(unlist(l))      ## Dealing with a single id
    
    ## Retrieve the entity
    s <- synapser::synGet( id, downloadFile=FALSE )

    ## Terminate recursion at the Project level
    if( grepl( "Project", s$properties$entityType ) ) return(id)

    ## Recurse up the chain
    c( id, synAncestry( s$properties$parentId ) )
}

#' Retrieve child entities
#' 
#' A wrapper around synGetChildren() for multiple Synapse IDs
#' 
#' @param ... One or more Synapse IDs for which children should be retrieved
#' @param .fullInfo set to TRUE to retrieve the full range of information for each child (default: FALSE)
#' @return A vector of child instance IDs if .fullInfo is FALSE. A data frame with all relevant info, if .fullInfo is TRUE.
#' @export
synChildren <- function(..., .fullInfo = FALSE)
{
    ## Split up computation, if multiple IDs are provided
    l <- purrr::flatten(list(...)) %>% purrr::set_names()
    if( length(l) < 1 ) stop( "Please provide at least one synapse ID" )
    if( length(l) > 1 ) return( purrr::map(l, synChildren, .fullInfo=.fullInfo) )

    ## Handle a single id
    id <- unname(unlist(l))
    if( !isSynID(id) ) stop( paste(id, "is not a valid Synapse ID") )
    ch <- synapser::synGetChildren( id )$asList()
    if( length(ch) == 0 ) return(ch)

    ## Pull everything into a single data frame
    res <- purrr::map( ch, as.data.frame, stringsAsFactors=FALSE ) %>% dplyr::bind_rows()
    if( .fullInfo ) return(res)
    purrr::set_names( res$id, res$name )
}
                            
