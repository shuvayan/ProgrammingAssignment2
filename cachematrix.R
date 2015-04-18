## The functions will take in a matrix and calculate it's inverse.In case of repeated inversion of the same matrix,
## the stored value will be retrieved.

## Creates a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) inv <<- solve
        get_inverse <- function() inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## Calculates the inverse of the matrix above.If the matrix has not changed from earlier calculation,it returns the cached value.

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}

