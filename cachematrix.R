## These functions compute and then cache the inverse of a matrix to reduce 
##   costly computations. 

## Function makeCacheMatrix cache the matrix 'x' in memory.
##   It returns NULL if the cache is empty or or the matrix length have changed.

makeCacheMatrix <- function(x = matrix()) {
    # Get the cached matrix if it exists
    if (exists("CacheMatrixSolved", envir= .GlobalEnv)) {
        m <- mget("CacheMatrixSolved", envir= .GlobalEnv)
        l <- mget("CacheMatrixLength", envir= .GlobalEnv)
        # Check if the matrix has changed
        if (l != length(x)) {
            m <- NULL
        }
    } else {
        m <- NULL
    }
    
    # Cache the Solved matrix
    setinverse <- function(solve) {
        m <- solve
        assign("CacheMatrixSolved", m, envir= .GlobalEnv)
        assign("CacheMatrixLength", length(m), envir= .GlobalEnv)
    }
    
    # Return the solved matrix
    getinverse <- function() m
    
    list(setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve returns the inverse of the matrix 'x'.
##   It will compute the inverse if it is not cached.
##   The cache is managed by the makeCacheMatrix function.

cacheSolve <- function(x) {
    # Get the cached matrix
    m <- makeCacheMatrix(x)$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Calculate the inverse of 'x' and cache it
    m <- solve(x)
    makeCacheMatrix(x)$setinverse(m)
    m
}


## My test data ##
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# h8 <- hilbert(8); h8
# solve(h8)
# cacheSolve(h8)




