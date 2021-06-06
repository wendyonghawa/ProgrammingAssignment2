## Below are two functions that will store a matrix and cache its inverse.

## makeCacheMatrix creates a list containing a function to: set and get the value of the matrix; set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve will produce the inverse of a matrix returned by the function makeCacheMatrix above.
## If the cache has been calculated, it will immediately return the inverse.
## If the cache has not been calculated, it will do set the inverse of the matrix using setinverse.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
