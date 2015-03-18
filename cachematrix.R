##---------------------------------------------------------------------------
## makecacheMatrix function creates a "matrix" object with needed 
## additional properties and methods for caching 

## cacheSolve function checks if it has already inverted matrix in its 
## x parameter property and if not, does the calculations and puts inverted 
## matrix in
## a property of x parameter using setCache method then returns inverted 
## matrix as a result
## otherwise just returns already "cached" inverted matrix
## benefits - may save time when working with matrices with sizes like 
## 1000*1000 or bigger. 
## 
## Example of usage:
## 
## m1 <- makeCacheMatrix(matrix(rnorm(1000000), nrow = 1000, ncol = 1000, 
##                                                       byrow = TRUE))
## m2 <- cacheSolve(m)
## m2 <- cacheSolve(m)
##---------------------------------------------------------------------------


## function makeCacheMatrix(x = matrix()) 
## parameters:
##      x  - is a matrix. for use in cacheSolve matrix must be invertible
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCache <- function(solve) m <<- solve
        getCache <- function() m
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)     

}


## function cacheSolve(x) 
## parameters:
##      x  - is a "matrix" object created by## makeCacheMatrix function

cacheSolve <- function(x, ...) {
      
        
        ## checking if we have our inverted matrix already,
        ## if so, just return it as a result
        m <- x$getCache()
        if(!is.null(m)) {

                message("getting cached inverted matrix!")
                return(m)
        }
        ## we do not have inverted matrix in the "cache", so we
        ## need to invert it and put into our "cache" 
        data <- x$get()
        m <- solve(data,...)
        x$setCache(m)
        m
        
}
