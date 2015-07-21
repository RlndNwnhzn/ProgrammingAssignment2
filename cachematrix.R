## This pair of functions define a "matrix" object and an operation
## (calculation of the inverse) on that object. The inverse will be
## saved in the object so it has to be calculated only once.

## This function creates the "matrix" object (implemented as a list) 
## that can cache and retrieve a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## This functions returns the inverse of the "matrix" object returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed) it is retrieved from cache.
## Otherwise the inverse is calculated and cached.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}