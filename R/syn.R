## Additional functionality for synapser
##
## by Artem Sokolov

#' Identifies if its argument is a valid Synapse ID
#'
#' @param ids A character vector containing one or more strings to identify.
#' @return Logical values indicating whether each element of the input vector is a valid Synapse ID.
#' @examples
#' isSynID( c("syn1234", "syn", "syn11ab") )
#' # [1]  TRUE FALSE FALSE
isSynID <- function( ids )
{ grepl( "^syn[1-9]+$", ids ) }

