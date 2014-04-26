## Put comments here that give an overall description of what your
## functions do

## function, makeCacheMatrix creates a special "matrix"
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y = matrix()) { #set a new matrix
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes invert matrix of a special "matrix" (objects generated with makeCacheMatrix) 
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        return(inverse)
}



