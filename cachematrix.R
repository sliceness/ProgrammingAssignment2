## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ##1. set the value of the matrix
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    
    ##2. get the value of the matrix
    get <- function() x
    
    ##3. set the value of the inverse
    setinverse <- function(solve) i <<- solve
    
    ##4. get the value of the inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    ##If inverse is already cached, return the inverse
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    data <- x$get()

    ##Otherwise, solve for the inverse
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
