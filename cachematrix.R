## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y     ## Set x to input
        i <<- NULL  ## Clear cached inverse matrix
    }
    get <- function() x
    setSolve <- function(inv) i <<- inv
    getSolve <- function() i
    
    # List attributes of x
    list(set = set, get = get, 
         setSolve = setSolve, 
         getSolve = getSolve)
}


## This function computes the inverse of the special matrix returned by 
## "makeCahceMatrix".  If the inverse has already been calculated (and the
## matrix has not changed), then "cacheSolve" should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getSolve()
    
    ## IF solve() has already been cached
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## ELSE call solve and cache
    data <- x$get()
    i <- solve(data, ...)
    x$setSolve(i)
    
    ## Display inverse matrix of x
    i
}
