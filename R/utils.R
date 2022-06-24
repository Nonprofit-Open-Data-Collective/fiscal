###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"

# remove double quotes
rm_quote <- function( x ) {
  gsub( "\"", "", string )
}

# %notin% operator
`%notin%` <- Negate( `%in%` )