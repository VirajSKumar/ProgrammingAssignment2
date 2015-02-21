## The functions below cache the inverse of a matrix to avoid repeated
## computation. All matrix inputs are assumed to be invertible. 

## Creates a "matrix", i.e. a list with four functions to set and get the 
## value of the matrix and the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}


## Computes the inverse of the "Matrix" created by the makeCacheMatrix function.
## If the inverse has been computed already, gets it from the cache. Else, 
## computes the value of the inverse and sets it in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
            message("getting cached data")
            return(i)
        }
        mat <- x$get()
        i <- solve(mat,...)
        x$setinverse(i)
        i
}
