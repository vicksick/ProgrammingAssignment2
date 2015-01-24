## Modified by Victor Martin
## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Given a matrix x, stores the value in the environment
## and resets m value. Creates setinv and getinv functions accessed when
## solving the matrix.

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


## cacheSolve: Given a cacheMatrix function checks m value of the enviornment.
## If m does not exist --> calculates it using solve and stores it via
## setinv function.

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
