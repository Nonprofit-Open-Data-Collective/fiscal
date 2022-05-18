# fiscal 

An R package for calculating nonprofit fiscal health accounting metrics from IRS 990 data. 


## Install the package

```r
devtools::install_github( 'nonprofit-open-data-collective/fiscal' )
```

## Test the package 

```r
library( fiscal )
help( get_dar )  # function documentation 

# test the debt-to-asset ration function
x1 <- rnorm(1000,100,30)
x2 <- rnorm(1000,200,30)
x2[ c(15,300,600) ] <- 0
dat <- data.frame(x1,x2)

d <- get_dar( df=dat, debt="x1", assets="x2" )
head( d )
```

**Notes on package creation** (internal use during package development) 

```r
library(devtools)
getwd()   # navigate to one level above "fiscal" folder

# step 1
usethis::create_package( "fiscal" )

# step 2 move R script to montyhall/R folder
# after completing documentation fields 

# step 3
setwd( "fiscal" )
devtools::document()

# step 4
setwd( ".." )
devtools::install( "fiscal" )
library( fiscal )
help( get_dar )

# test function
x1 <- rnorm(1000,100,30)
x2 <- rnorm(1000,200,30)
x2[ c(15,300,600) ] <- 0
dat <- data.frame(x1,x2)
d <- get_dar( df=dat, debt="x1", assets="x2" )
head( d )
```

