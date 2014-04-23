### Assignment: Caching the Inverse of a Matrix
## Create cache matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinv <- function(solve) m <<- solve
            getinv <- function() m
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}

## compute the inverse of a matrix unless it's been done before for this matrix
## in which case, return the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


            m <- x$getinv()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinv(m)
            m

}
