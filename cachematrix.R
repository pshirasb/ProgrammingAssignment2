# Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the inverse to null
    inv     = NULL

    # Get/Set value of x
    get     = function() x
    set     = function(y) { 
                 x   <<- y
                 inv <<- NULL
              }   

    # Get/Set inverse of x
    getinv  = function() inv
    setinv  = function(z) inv <<- z

    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
# NOTE: x is assumed to be invertible
cacheSolve <- function(x, ...) {

    # Check the cache for inverse
    inv = x$getinv()
    
    # if cache is empty, compute the inverse and cache it
    if(is.null(inv)) {
        inv = solve(x$get(), ...)
        x$setinv(inv)
    }

    # return the inverse
    return(inv)   
}
