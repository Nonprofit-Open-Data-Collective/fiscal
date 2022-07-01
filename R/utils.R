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

# coerce variables to numeric if not numeric

coerce_numeric <- function( d, var) {
  
  these <- which( colnames( d ) %in% var )
  
  n <- sum( sapply( d[ these ], function( x ) is.numeric( x)  ) )
  
  # coerce
  if(  n < length( these ) ){
    warning( paste0( "At least one of the provided numerator variables was not of object class numeric. ", length( these )-n, " variables were ( was)  coerced to numeric." ) )
    
    d[ var ] <- data.frame( sapply( d[ var ], function( x ) as.numeric( as.character ( x)  ) ) )
  }
  
  return( d )
}

# Example:
# d1<-data.frame( x1 = as.character( rnorm( 1000) ) , x2 = as.character( rnorm( 1000) ) ) 
# class( d2$x1 );class( d2$x2 )
# d2<-coerce_numeric( d=d1, var=c( 'x1','x2') ) 
# class( d2$x1 );class( d2$x2 ) 