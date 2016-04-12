## makeCacheMatrix: Creating a matrix object that can cache its inverse
## cacheSolve: retunrs the inverse of the matrix from cache, if already ccomputed by..
## ..makeCacheMatrix - or computes inverse if it has not yet been computed and also sets 
## the inverse solution in the cache

# makeCacheMatrix creates a special matrix object, a list containing a function to
# 1) set the values of the matrix
# 2) get the values of the matrix
# 3) set the values of the inverse of the matrix 
# 4) get the values of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve returns the inverse of the matrix object created with the function makeCacheMatrix
# First checks if the inverse has alreasy been computed, then it gets it from the cache
# and does not recompute. If the solution does not yet exits, it calculates the inverse and
# also sets the inverse solution in the cache, by the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
