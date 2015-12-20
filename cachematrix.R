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
## matrix and caches that inverse for future calls, if necessary.
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


## Test method for matrix caching
basicCacheTest <- function(){
    #Create 1000 X 1000 matrix
    randomMatrix <- matrix(sample.int(2^32, 1000000), nrow = 1000, ncol = 1000)
    
    #Create cache object
    cacheObject <- makeCacheMatrix(randomMatrix)
    
    inverseMatrix <- NULL
    
    #Store the amount of time it takes to retrieve the
    #inverse matrix when nothing is cached
    nonCacheTime <- system.time({
        inverseMatrix <<- cacheSolve(cacheObject)
    })
    print(nonCacheTime)
    
    cachedInverse <- NULL
    #Store the amount of time it takes to retrieve the
    #inverse matrix when it is cached
    cacheTime <- system.time({
        cachedInverse <<- cacheSolve(cacheObject)
    })
    print(cacheTime)
    
    #Check that the two inverses are in fact the same.
    if(!identical(inverseMatrix, cachedInverse)){
        stop("Original inverted matrix and matrix retrieved from cache are not the same.")
    }
          
    if(nonCacheTime[3] <= cacheTime[3]){
        stop("Retrieving inverted matrix from cache took as long or longer than original inversion.")
    }
    
}
    