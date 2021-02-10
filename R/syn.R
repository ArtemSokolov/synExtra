## Additional functionality for synapser
##
## by Artem Sokolov

SYN_REGEX = c(
    "with_version" = "^syn[0-9]+(\\.[0-9]+)?$",
    "plain" = "^syn[0-9]+$"
)

#' Check validity of Synapse IDs
#'
#' Identifies if its argument is a valid Synapse ID character string
#'
#' @param ... One or more Synapse IDs. Accepts individual IDs, vectors or lists.
#' @param .with_version Allow an optional version suffix requesting a specific version (e.g. syn324.4)
#' @return Logical values indicating whether each element of the input is a valid Synapse ID.
#' @examples
#' isSynID( "syn1234", "syn", "syn123ab", "syn123.5" )
#' # [1]  TRUE FALSE FALSE FALSE
#' isSynID( "syn1234", "syn", "syn123ab", "syn123.5", .with_version = TRUE )
#' # [1]  TRUE FALSE FALSE TRUE
#' isSynID( list( mtcars, 123, "syn123" ) )
#' # [1] FALSE FALSE  TRUE
#' @export
isSynID <- function( ..., .with_version = FALSE )
{
    version <- if (.with_version) "with_version" else "plain"
    ids <- purrr::flatten(list(...))
    purrr::map_lgl( ids, is.character ) & grepl(SYN_REGEX[version], ids)
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

#' Retrieve parentId
#'
#' Retrieves parentIds for one or more of the provided Synapse IDs
#'
#' @param ... One or more Synapse IDs for which parent IDs must be retrieved
#' @return Named character vector that maps the input Synapse IDs their parentIds
#' @examples
#' \dontrun{
#' synParent("syn15663039", "syn1695362" )
#' #   syn15663039    syn1695362
#' # "syn15673834"  "syn1695324"
#' }
#' @export
synParent <- function(...)
{
    ## Split up computation, if multiple IDs are provided
    l <- purrr::flatten(list(...)) %>% purrr::set_names()
    if( length(l) < 1 ) stop( "Please provide at least one synapse ID" )
    if( length(l) > 1 ) return( purrr::map_chr(l, synParent) )

    ## Handle a single id
    id <- unname(unlist(l))
    if( !isSynID(id) ) stop( paste(id, "is not a valid Synapse ID") )

    ## Retrieve the entity and its parentId
    s <- synapser::synGet( id, downloadFile=FALSE )
    s$properties$parentId
}

#' Rename a synapse entity
#'
#' Assigns a new file name to the synapse entity identified by its ID
#'
#' @param sid Synapse ID of the entity to rename
#' @param newName New filename that should be assigned to the entity
#' @return Synapse ID of the entity (for integration with the %>% pipe)
#' @export
synRename <- function( sid, newName )
{
    ## Retrieve the entity
    s <- synapser::synGet( sid, downloadFile=FALSE )

    ## Assign the new name
    s$properties$name <- newName

    ## Store the entity back
    s <- synapser::synStore(s)
    sid
}

