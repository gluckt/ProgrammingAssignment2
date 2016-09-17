## Creates and returns a list containing functions to 
## set the value and inverse of the matrix and functions 
## to get these values

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of a matrix
## If the inverse has already been calculated, returns it from the cache
## and skips the calculation

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    return(inv)
}
