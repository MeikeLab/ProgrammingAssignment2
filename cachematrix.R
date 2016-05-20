## makeCacheMatrix consists of 4 functions: set, get, setinverse
## and getinverse. "set" sets the matrix and stores its value in
## x, "get" returns the matrix by calling x, "setinverse" stores 
## the value for the matrix´ inverse, "getinverse" returns the 
## matrix´ inverse.


makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
         set <- function(y) {
                 x <<- y
                 i <<- NULL
         }
         get <- function() x
         setinverse <- function(inverse) i <<- inverse
         getinverse <- function() i
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
 }


## cacheSolve computes the inverse of the "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.
 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
