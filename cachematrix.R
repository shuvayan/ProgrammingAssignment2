## The inverse of a matrix will be calculated.In case the same matrix has been used before and the inverse calculated,the
## cached value will be displayed.

## Creates a special matrix function that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        # Create the set_inverse and get_inverse functions
        set_inverse <- function(solve) inv <<- solve
        get_inverse <- function() inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## Calculate the inverse of the matrix returned by the above function.In case cached value of the matrix is available,
##that is returned.

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

