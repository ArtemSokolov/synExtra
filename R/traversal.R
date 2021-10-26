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
#' # $syn15663039
#' # [1] "syn15663039" "syn15673834" "syn15673837" "syn12180284"
#' #
#' # $syn1695362
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
    if( grepl( "Project", s$properties$concreteType ) ) return(id)

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
#' @examples
#' \dontrun{
#' synChildren( "syn6185321", "syn5049679" )
#' # $syn6185321
#' #          hairpin fasta miRNA mature structure
#' #           "syn6185324"           "syn6185325"
#' #
#' # $syn5049679
#' # hsa_MTI_6.1.csv
#' #    "syn5049680"
#' }
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

#' Traverse a synapse path
#'
#' Starting from the provided synapse ID, traverses descendants by name
#'
#' @param sid Synapse ID of the starting entity
#' @param ... One or more names for constructing a path. Accepts individual names, vectors and lists
#' @return Synapse ID of the "plucked" entity
#' @examples
#' \dontrun{
#' synPluck( "syn1773110", "mRNA", "Counts", "htseq-count", "H9.144.7.7.txt" )
#' # [1] "syn2822560"
#' }
#' @export
synPluck <- function( sid, ... )
{
    ## Retrieve children of the starting node
    s <- synChildren( sid )
    if( length(s) == 0 )
        stop( paste(sid, "has no children") )

    ## Handle terminal cases
    l <- purrr::flatten(list(...))
    if( length(l) == 0 )
        stop( "Please provide at least one name to index" )

    ## Determine synapse ID of the next child
    if( !(l[[1]] %in% names(s)) )
       stop( paste(l[[1]], "is not a child of", sid) )
    chid <- purrr::pluck(s, l[[1]])
    if( length(l) == 1 )
        return( chid )

    ## Recurse
    synPluck( chid, l[-1] )
}

#' Create a synapse directory hierarchy
#'
#' Starting from the provided synapse ID, traverse descendants by name like
#' [synPluck()]. If a child doesn't exist, create a directory with that name.
#' If `.recursive` is set, also create intermediate directories as necessary.
#'
#' Returns Synapse ID of final child like [synPluck()] if the directory already
#' exists.
#'
#' @param sid Synapse ID of the starting entity
#' @param ... One or more names for constructing a directory hierarchy.
#'   Accepts individual names, vectors and lists
#' @param .recursive Whether to create intermediate directories if they don't exist (default: FALSE)
#' @return Synapse ID of the "plucked" entity
#' @examples
#' \dontrun{
#' synMkdir( "syn1773110", "mRNA", "Counts", "htseq-count" )
#' # [1] "syn2822494"
#' }
#' @export
synMkdir <- function( sid, ..., .recursive = FALSE )
{
    l <- purrr::flatten(list(...))
    if( length(l) == 0 )
        stop( "Please provide at least one name to index" )

    for (i in seq_along(l)) {
        s <- synChildren( sid )
        if ( l[[i]] %in% names(s) ) {
            sid <- unname(s[l[[i]]])
            next
        }
        if ( i != length(l) && !.recursive ) {
            stop( paste(l[[i]], "is not a child of", sid) )
        }
        sid <- synapser::Folder(l[[i]], parent = sid) %>%
            synapser::synStore() %>%
            purrr::chuck("properties", "id")
    }

    sid
}

#' Traverse a synapse path with wildcard matching
#'
#' Starting from the provided synapse ID, traverses descendants by name
#' that can contain wildcards (*) that match an arbitrary number of characters.
#'
#' Details about glob wildcard character matching are available in the
#' [Wikipedia article](https://en.wikipedia.org/wiki/Glob_(programming)).
#'
#' @param sid Synapse ID of the starting entity
#' @param ... One or more names for constructing a path. Accepts individual names, vectors and lists
#' @param .nested Return synapse paths as flat vector (FALSE) or as nested list
#'   following the directory structure (TRUE)
#' @return Synapse IDs of the "plucked" entities
#' @examples
#' \dontrun{
#' synGlob( "syn26348949", "Runs", "*", "meta", "*.csv" )
#  # [1] "syn26349230" "syn26349250" "syn26349251" "syn26349268"
#' }
#' @export
synGlob <- function( sid, ..., .nested = FALSE )
{
  ## Retrieve children of the starting node
  s <- synChildren( sid )
  if( length(s) == 0 )
    return( character() )

  ## Handle terminal cases
  l <- purrr::flatten(list(...))
  if( length(l) == 0 )
    stop( "Please provide at least one name to index" )

  ## Determine synapse ID of the next children
  matches <- grep( glob2rx(l[[1]]), names(s), value = TRUE )
  chids <- s[matches]
  if( length(l) == 1 )
    return( chids )

  ## Recurse
  r <- purrr::map( chids, synGlob, l[-1], .nested = .nested ) %>%
    ## Remove empty nodes
    purrr::compact()
  if (!.nested && !is.null(r))
    ## Remove names
    r <- purrr::set_names(r, NULL) %>%
      unlist()
  r
}
