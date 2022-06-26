###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"

# remove double quotes
rm_quote <- function( x ) {
  gsub( "\"", "", x )
}

# %notin% operator
`%notin%` <- Negate( `%in%` )