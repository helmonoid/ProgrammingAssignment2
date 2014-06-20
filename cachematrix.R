## This file contains two functions to create a special matrix
# object and cache its inverse.In order to use it, create a 
## cacheable matrix, m<- makeCacheMatrix( ). initialize with a 
## matrix that is easy to inspect e.g. m$set( matrix( c(0, 4, 4, 0 ), 2, 2))
## m$get() retrives the matrix part of the object.
## test the inverse cacher with cacheSolve(m), repeat cacheSolve(m)
## It shall display: "getting cached data" and print the cached data.

## ----------------------------------------------------------------------------

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(y = matrix()) {
        
        m <- NULL
        
        set <- function(matrix){
                y <<- matrix
                m <<- NULL
        }
        get <- function(){
                y
        }
        setinverse <- function(inverse){
                m <<- inverse
        }
        getinverse <- function() {
                m
        }
        
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then cachesolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        y <- x$getinverse()
        
        if (!is.null(y)){
                message("getting cached data")
                return(y)
        }
        data <- x$get()

        y <- solve(data, ...)
        
        x$setinverse(y)
        
        y
}
        

