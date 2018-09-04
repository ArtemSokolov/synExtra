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
    dl <- function( id )
    { synapser::synGet( id, downloadLocation = dloc, ... )$path }

    ## Apply it to all synapse IDs
    function( ... )
    { purrr::flatten(list(...)) %>% purrr::map_if( isSynID, dl ) %>% purrr::flatten_chr() }
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
    l <- purrr::flatten(list(...))
    if( length(l) > 1 ) return( purrr::map(l, synAncestry) )
    id <- unlist(l)	## Dealing with a single id
    
    ## Retrieve the entity
    s <- synapser::synGet( id, downloadFile=FALSE )

    ## Terminate recursion at the Project level
    if( grepl( "Project", s$properties$entityType ) ) return(id)

    ## Recurse up the chain
    c( id, synAncestry( s$properties$parentId ) )
}

#' Streamlined version of synQuery()
#'
#' An intuitive interface for composing Synapse queries
#'
#' @param ... Arguments defining the query. Unnamed arguments make up the fields to be selected.
#' Named arguments define the query conditions.
#' @param .echo Whether to display the final query string. (Default: TRUE)
#' @return The function always returns a data frame with columns defined by the unnamed arguments. If the query returned no results, the data frame will have zero rows (but non-zero number of columns).
#' @examples
#' \dontrun{
#' synq( "id", "name", "projectId", parentId = "syn15673834", type = "dataset" )
#' # select id,name,projectId from entity where "parentId"=="syn15673834" and "type"=="dataset" 
#' # # A tibble: 1 x 3
#' #   projectId id          name      
#' #       <dbl> <chr>       <chr>     
#' # 1  12180284 syn15663039 mtcars.csv
#'
#' synq( "id", "name", projectId = "12180284", type="Nonexistent", .echo=FALSE )
#' # # A tibble: 0 x 2
#' # # ... with 2 variables: id <chr>, name <chr>
#' 
#' }
#' @importFrom magrittr %>%
#' @export
synq <- function(..., .echo = TRUE)
{
    ## Capture the arguments
    A <- tibble::enframe(list(...), "name", "value")

    ## Unnamed arguments make up the "what" portion of the query
    .what <- A %>% dplyr::filter( name == "" ) %>% with( stringr::str_flatten(value, ",") )

    ## Named arguments make up the "where" portion of the query
    .where <- A %>% dplyr::filter( name != "" ) %>%
        dplyr::mutate_at( "value", purrr::map_if,
                         is.character, ~stringr::str_c( '"', .x, '"' ) ) %>%
        with( purrr::map2(name, value, ~stringr::str_c('"', .x, '"==', .y)) ) %>%
        stringr::str_flatten( " and " )

    ## Compose the final query string
    qq <- stringr::str_c( "select ", .what, " from entity where ", .where )
    if( .echo ) cat( qq, "\n" )

    ## Perform the query
    QQ <- synapser::synQuery(qq)$results %>% dplyr::bind_rows

    ## If query returned no results, compose a zero-row data frame
    ## Columns are determined by the requested fields
    if( nrow(QQ) == 0 )
        dplyr::filter( A, name == "" )$value %>% purrr::set_names() %>%
                                         purrr::map_dfc( ~character() )

    ## Drop "entity." from the column names
    else dplyr::rename_all( QQ, stringr::str_sub, 8 )
}

#' Upload and annotate a file
#'
#' The function uploads a local file and annotates it with fields provided as named arguments
#'
#' @param .fn Filename of the local file to upload
#' @param .dest Synapse ID of the destination Folder
#' @param .forceVer Maps to forceVersion argument of synStore(), which is defined as
#' "Indicates whether the method should increment the version of the object even if
#' nothing has changed. Defaults to TRUE."
#' @param ... Named arguments specifying key-value pairs to serve as annotations
#' @return Annotated synapse entity object
#' @examples
#' \dontrun{
#' synUpload( "mtcars.csv", "syn12180284", Field1 = c("abc","def"), Field2 = 123 )
#' }
#' @importFrom magrittr %>%
#' @export
synUpload <- function( .fn, .dest, ..., .forceVer = TRUE )
{
    ## Compose key-value annotations
    A <- list(...)
    if( length(A) > 0 && (is.null(names(A)) || any(names(A) == "")) )
        stop( "All annotations must be provided as named arguments" )

    ## Compose, annotate and synStore() the file entity
    synapser::File( .fn, parent = .dest, annotations=A ) %>%
        synapser::synStore( forceVersion = .forceVer )
}
