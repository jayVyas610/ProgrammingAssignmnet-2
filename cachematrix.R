## set:Creates a matrix x
##get: stores the matrix x
##setinverse: assigns the inverse to m
##getinverse: stores the inverse m into cache
## matix has 4 functions: set, get, setinverse, getinverse


## The function is essentially same as the one in example, 
##this one creates a matix
## and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

##calculates the inverse of a matrix
##checkes if the matrix is already inverse 
##? return from chache : clculates inverse and stores(sets) in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
