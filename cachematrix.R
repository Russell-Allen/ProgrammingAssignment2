## The functions in this file provide the ability to compute
## and store the inverse of a matrix.  Calling code may get
## the inverse as frequently as desired, and this code will
## only compute the inverse once.
##
## Example Usage:
##
## >  m <- matrix(rnorm(4),2,2)      ## example matrix, m
## >  cachedM <- makeCacheMatrix(m)  ## create a cached version
## >  cacheSolve(cachedM)            ## first call computes
## Drat, having to compute the inverse.  Please hold...
##           [,1]       [,2]
## [1,] 0.3715412  0.4092523
## [2,] 0.3983393 -1.4062522
## > cacheSolve(cachedM)             ## 2nd call, use cached value.
##           [,1]       [,2]
## [1,] 0.3715412  0.4092523
## [2,] 0.3983393 -1.4062522
## > cacheSolve(cachedM)             ## use cached value... etc.
##           [,1]       [,2]
## [1,] 0.3715412  0.4092523
## [2,] 0.3983393 -1.4062522



## makeCacheMatrix function creates a pairwise cache of a matrix 
## and its inverse.  The primary matrix value can be set or
## retrieved, and its inverse can be set or retrieved as well.
##
## Note, this function caches the supplied values only.  It 
## does not calculate the inverse or otherwise validate that 
## it is as described.
##
## Setting the primary matrix value, via set(), will store
## the provided value as the primary matrix and clear the cached
## inverse matrix (setting it to NULL.)
## Getting the priamry matrix will return the last value set
## via set().
## Getting and setting the inverse will get and set the cached
## inverse matrix value (respectively.)  The inverse matrix will
## be NULL if it has not been set since the last call to set().

makeCacheMatrix <- function(x = matrix()) {
    
    ## PRIVATE DATA ##
    
    ## inverseOfMatirx is either the inverse of the set matrix
    ## or NULL if the inverse has not been calculated yet.
    inverseOfMatirx <- NULL
    
    
    ## FUNCTIONS ##
    
    ## Sets the matrix that this function is caching and 
    ## resets the cached inverseOfMatirx to NULL.
    set <- function(y) {
        x <<- y
        inverseOfMatirx <<- NULL
    }
    
    ## Returns the cached matrix.
    get <- function() x
    
    ## Sets the provided value as the cached inverseOfMatirx
    ## value.  This val
    setInverse <- function(invMatrix) inverseOfMatirx <<- invMatrix
    
    ## Returns the cached inverseOfMatirx or NULL if it has
    ## not been set yet.
    getInverse <- function() inverseOfMatirx
    
    
    ## RESULT ##
    
    ## Return a list with 4 functions as values.  This list
    ## creates a closure such that these functions retain
    ## access to inverseOfMatirx and x as managed above.
    list(
        set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Given a matrix cacheSolve returns the inverse of that matrix.
## The results of the inverse are cached such that future calls to
## this function return the cached result, and thus avoid the overhead
## of computing the inverse again.
##
## Additional parameters to this function will be passed through to
## the solve(x, ...) function.

cacheSolve <- function(x, ...) {
    
    ## inverse will contain our result.  Set it to the cached
    ## value first.
    inverse <- x$getInverse()  ## may be NULL
    
    ## Only if NULL: compute the inverse and cache it.
    if( is.null(inverse) ) {
        message("Drat, having to compute the inverse.  Please hold...")
        data <- x$get()  ## get primary matrix
        inverse <- solve(data, ...)  ## compute inverse
        x$setInverse(inverse)  ## cache inverse results
    }

    inverse  ## 'Return' the inverse as the result.
}
