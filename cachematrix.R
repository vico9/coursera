

## function, makeCacheMatrix creates a special "matrix"
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL

	  
        #set function: set a new matrix, reset the inverse
        set <- function(y = matrix()) { 
                x <<- y                
                inverse <<- NULL
        }

        #get function: return the matrix
        get <- function() x

        #setinverse function: sets the inverse 
        setinverse <- function(inv) inverse <<- inv

        #getinverse function: returns the inverse
        getinverse <- function() inverse

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes invert matrix of a special "matrix" (objects generated with makeCacheMatrix) 
cacheSolve <- function(x, ...) {
        
        #check, if inverse of x is already computed 
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        #else
        #compute and return the inverse of 'x'
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        return(inverse)
}



