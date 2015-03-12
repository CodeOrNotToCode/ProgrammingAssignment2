## These two functions have the purpose of calculating an inverse matrix once and
## preventing multiple calculations even if CacheSolve is called again by instead
## returning a cached solution.


## makeCacheMatrix(x = matrix()) Creates a list containing functions to be called
## by CacheSolve. Main purpose is to store the original matrix and to cache 
## an inverse matrix created by CacheSolve
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                     ## inv is a local variable
        set <- function(y){                             ## x in makeCacheMatrix environment
                x <<- y                                 
                inv <<- NULL
        }
        get <- function() x                             ## provides original matrix
        setinv <- function(inverse)  inv <<- inverse    ## caches inverse matrix
        getinv <- function() inv                        ## provides inverse matrix
        
        ## return list object that can be used by CacheSolve
        list(set = set, get = get, setinv = setinv, getinv = getinv) 
}



## cacheSolve() takes a list x (created by makeCacheMatrix) and returns its inverse
## either by reading the cached solution by calling x$getinv() or - if no previous
## solution exists - by calculating (and then caching) the inverse matrix.

cacheSolve <- function(x, ...) {
        
        ## Check whether CacheMatrix contains an entry for inverse matrix
        ## If yes, return cached inverse matrix
        inverse = x$getinv()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        
        ## Else get original matrix and calculate inverse matrix... 
        data <- x$get()
        inverse <- solve(data, ...)
        
        ## ...store solution in CacheMatrix...
        x$setinv(inverse)
        
        ## ...and return inverse matrix
        inverse        
}
