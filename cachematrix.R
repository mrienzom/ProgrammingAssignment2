## Together, these functions allow the user to create a square 
## matrix, invert the matrix, and keep the inverse cached until
## the original matrix is overwritten. This minimizes the number
## of required matrix inversion computations.

## The first function creates a list of four functions that (1)
## store a matrix, (2) return the stored matrix, (3) store a 
## computed inverse matrix, (4) return the stored inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) z <<- inverse
        getinverse <- function() z
        list(set = set, get = get, setinverse = setinverse,
                getinverse = getinverse)
}


## This function uses the functions created above to check whether 
## the inverse of the stored matrix has already been cached. If so,
## it retrieves the cached inverse. If not, it computes the inverse, 
## caches it, and then returns it.

cacheSolve <- function(x, ...) {
        z <- x$getinverse()
        if (!is.null(z)) {
                message("getting cached inverse...")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}
