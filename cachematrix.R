#' A caching version of matrix inverse function.

#' Create a special matrix object that caches the inverse of the matrix value,
#' with functions to get/set its matrix value, and getSolve/setSolve its
#' inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setSolve <- function(sv) cache <<- sv
        getSolve <- function() cache
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


#' A function to compute the inverse of a given matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m <- solve(x$get())
        x$setSolve(m)
        m
}