# The following two functions will be used to provide cached inverse matrix. So we just
#calcualte inverse of a matrix only once and subsequent call to the same matrix return
#cached result


#This function creates a list of function which can cache its inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
    invResult <- NULL
    set <- function(y){
        x <<- y
        invResult <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) invResult <<- inverse
    getinverse <- function() invResult
    list(set = set, get = get, setinverse = setinverse,getinverse= getinverse)
}

# This function return inverse of a matrix. The result will be calculated or returned
# from cache.

cacheSolve <- function(x, ...) {
        
    invResult <- x$getinverse()
    if(!is.null(invResult)){
        message("getting cached data")
        return (invResult)
    }
    data <- x$get()
    invResult <- solve(data, ...)
    x$setinverse(invResult)
   
    ## Return a matrix that is the inverse of 'x'
    invResult
}
