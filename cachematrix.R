## My functions are for cacheing the inverse of a matrix, just like in the example vector mean caching

## This function creates a matrix, that can cache its inverse calcualted with solve function

makeCacheMatrix <- function(x = matrix()) {
        ## setting the value of the matrix
        i<- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## getting the value of the matrix
        get <- function() x
        
        ## setting the inverse of the matrix
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        ## getting the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the inverse of the matrix        
        i <- x$getinverse()
        
        ## check if there is the matrix   
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        ## set the inverse of the matrix 
        x$setinverse(i)
        i
}
