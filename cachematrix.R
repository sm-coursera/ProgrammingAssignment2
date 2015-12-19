## Put comments here that give an overall description of what your
## functions do


## Given a matrix this function returns a list containing
## two methods for accessing the original matrix passed in and
## two methods for accessing the cached matrix.
makeCacheMatrix <- function(x = matrix()) {
    # Intitialize cached matrix to NULL
    cachedMatrix <- NULL
    
    # Accessor for setting a new matrix value for the cache
    # object. The cached matrix must be reset to NULL since
    # the original value has changed.
    setMatrix <- function(newMatrix){
        x <<- newMatrix
        cachedMatrix <<- NULL
    }
    # Accessor for retrieving the original matrix value.
    getMatrix <- function() {x}
    
    # Accessor for setting the value in the cache.
    setCachedValue <- function(newCacheValue){
        cachedMatrix <<- newCacheValue
    }
    # Accessor for retrieving the cached value.
    getCachedValue <- function(){cachedMatrix}
    
    # Return a list that contains the functions necessary
    # to access the cache. Since we used the <<- operator 
    # to set the values we capture the original and cached
    # matrices in the environments of the functions.
    list(getMatrix = getMatrix, setMatrix = setMatrix,
         getCachedValue = getCachedValue, setCachedValue = setCachedValue)
}

## Given a matrix cache as returned by the makeCacheMatrix 
## function this function returns the inverse of the original 
## matrix and caches that inverrse for future calls, if necessary.
cacheSolve <- function(x, ...) {
    # Retrieve the cached matrix
    inverseOfX <- x$getCachedValue();
    # Check if the cache was null. If it was calculate the 
    # value and cache it
    if(is.null(inverseOfX)){
        inverseOfX <- solve(x$getMatrix())
        x$setCachedValue(inverseOfX)
    }
    #return the inverse
    inverseOfX
}

