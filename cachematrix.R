## Functions below create a special object to store a matrix and cache its inverse


## makeCacheMatrix creates a list containing the following functions
##      1. Set the value of the Matrix
##      2. Get the value of the Matrix
##      3. Set the Value of the Inverse of the Matrix
##      4. Get the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverse <<- solve
        getsolve <- function() inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cachesolve provides the inverse of the matrix created utilizing the function above
##      1. Checks to see if the inverse is in cache, if not, it calculates and returns the inverse of the Matrix
##      2. Inverse is in the cache, it returns the cached inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}
