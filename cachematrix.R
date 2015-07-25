
## The makeCacheMatrix function creates a special "matrix",
## which is a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # has the cached value or NULL if nothing is cached
        # initially set it to NULL
        i <- NULL
       
       # store a matrix 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # returns the stored matrix
    get <- function() x
    
    # cache the argument 
    setinverse <- function(inverse) i <<- inverse
    
    # get the cached value
    getinverse <- function() i
    
     # return a list.
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix"
## which is created with the makeCacheMatrix function.
## It first checks if the inverse has already been calculated
## Then the inverse from the cache 
## Else calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        # get the cached value
         i <- x$getinverse()
         
        # if a cached value exists return it
            if(!is.null(i)) {
                message("getting cached data")
                return(i)
            }
        # else get the matrix, caclulate the inverse and store it in
        # the cache
            data <- x$get()
            i <- solve(data, ...)
            x$setinverse(i)
        # return the inverse
                i
}
