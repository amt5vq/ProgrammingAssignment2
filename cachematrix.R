## These funcitons cache the inverse of a matrix, so we don't recalculate with
## each new input.  If the inverse is not cached, we calculate then cache it.

## This function caches the value of a matrix then retrieves it.
## It also sets the inverse for a given x matrix, then stores the four 
## functions as a list.

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) n <<- inverse
        getinverse <- function() n
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function verifies the value n, stores it in memory, then returns it.
## If the value is null, then it calculates the inverse of the function,
## then stores it

cacheSolve <- function(x, ...) {
        n <- x$getinverse()
                if(!is.null(n)) {
                        message("getting cached data")
                        return(n)
                }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
        n
}
