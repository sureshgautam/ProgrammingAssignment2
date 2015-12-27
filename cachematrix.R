## Caching the Inverse of a Matrix

## Assignment 2 for R Programming...

## makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve
cacheSolve <- function(x, ...) {
        inv = x$getinv()
        
        if (!is.null(inv)){
                message("getting cached data...")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
