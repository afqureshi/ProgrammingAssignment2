## This is combination of functions which cache and compute inverse of a matrix.
## This function makes a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the "matrix" returned by `makeCacheMatrix`, if the inverse has
## calculated, then`cacheSolve` retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}