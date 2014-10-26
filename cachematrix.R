## R Programming Assignment 2


## Create a special "matrix" that can cache its inverse.
## The sample implementation in the assignment is a flawed design as it exposes
## private internal details of the object and requires the caller to manage
## the cache.  Really there should be no setinverse(), only a getinverse()
## function that checks the cache and computes the inverse on demand.  I couldn't
## make this work because solve() behaves very strangely when nested inside an
## inner function.
makeCacheMatrix <- function(x = matrix()) {
    ## Cached inverse
    inverse <- NULL
    getinverse <- function() inverse
    setinverse <- function(v) {
        inverse <<- v
    }
    set <- function(y) {
        inverse <<- NULL
        x <<- y
    }
    get <- function() x
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## Return the inverse of our special cached matrix object.
## The skeleton we were provided uses ... in the function signature, implying
## that any extra arguments to cacheSolve should be passed to solve().  Logically
## this should require caching those arguments along with the inverse.  Changing
## the extra arguments can change the result of the solve() call, making the
## cached value incorrect for the changed extra arguments.  That complicates
## the assignment significantly, so I didn't do it.
cacheSolve <- function(x, ...) {
    ## Return the cached inverse if it is not null.
    if (!is.null(x$getinverse())) {
        message("retrieving cached inverse")
        return(x$getinverse())
    }
    ## Compute the inverse and cache it.
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    return(inv)
    rep
}


## Simple tests to make sure the caching works.
tests <- function() {
    m <- makeCacheMatrix(cbind(c(2, 0), c(0, -5)))
    message("test matrix")
    print(m$get())
    message("first solve (not in cache)")
    print(cacheSolve(m))
    message("second solve (cached)")
    print(cacheSolve(m))
    message("updated matrix, first solve (cache was cleared)")
    m$set(rbind(c(1, 2), c(3, 4)))
    print(cacheSolve(m))
    message("updated matrix, second solve (cached)")
    print(cacheSolve(m))
}
