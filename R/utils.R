###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"

# %notin% operator
`%notin%` <- Negate( `%in%` )

# coerce variables to numeric if not numeric

coerce_numeric <- function( d, vars) {
  
  these <- which( colnames( d ) %in% vars )
  
  n <- sum( sapply( d[ these ], function( x ) is.numeric( x)  ) )
  
  # first check that variables contain only digits, stop if not
  
  if ( sum( sapply( d[ vars ], function(x) sum( stringr::str_detect( x, '^([A-Za-z\\s]*)$' ), na.rm = T ) ), na.rm = T ) != 0 )
    { stop( "Non-digit characters detected in at least one of the columns specified. Ensure all variables contain only digits before executing the function." ) }
  
  # coerce
  if (  n < length( these ) ){
    warning( paste0( "At least one of the provided numerator variables was not of object class numeric. ", length( these )-n, " variables were ( was)  coerced to numeric." ) )
    
    d[ vars ] <- data.frame( sapply( d[ vars ], function( x ) as.numeric( as.character ( x)  ) ) )
    
    
  }
  
  return( d )
}

 # Example
 # d1 <- data.frame( x1 = as.character( rnorm( 1000) ) , x2 = as.character( rnorm( 1000) ) ) 
 # class( d1$x1 ) ; class( d1$x2 )
 # d2 <- coerce_numeric( d=d1, var=c( 'x1','x2') ) 
 # class( d2$x1 ) ; class( d2$x2 ) 
 # d3 <- coerce_numeric( d=d2, var=c( 'x1','x2') ) 
 # is.null( d3 )
 # Error Message:
 # d1[ 3, 'x2' ] <- 'p'
 # coerce_numeric( d=d1, var=c( 'x1','x2') ) 
