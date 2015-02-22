## In these functions the inverse of a given matrix is calculated and cached it in order to
## be looked up easily rather than recomputed.

## This function creates a list of functions used for setting and getting the cached value 
## of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Variable inv is initialised to NULL.
        set <- function(y) {
                ## Here the previous inverse is reinitialised when new data is introduced.
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## Here the initial matrix is returned.
        setinv <- function(solve) inv <<- solve ## This function caches the inverse for future use.
        getinv <- function() inv ## Here the cached inverse is returned.
        list(set = set, get = get, ## The list of subfunctions returned by makeCacheMatrix.
             setinv = setinv,
             getinv = getinv)        
}

## This function calculates and returns the inverse of a given matrix x.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()  ## Looking in the cache for a stored value of the inverse matrix.
        if(!is.null(inv)) {
                ## Here a message is shown if a cached value of the mean is found.
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ## Retrieve the initial matrix from makeCacheMatrix.
        inv <- solve(data, ...) ## The inverse is calculated here.
        x$setinv(inv) ## Here the calculated inverse is stored in cache.
        inv ## Returns the calculated inverse.
}