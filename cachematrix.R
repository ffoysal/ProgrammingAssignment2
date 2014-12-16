# The following two functions will be used to provide cached inverse matrix. So we just
#calcualte inverse of a matrix only once and subsequent call to the same matrix return
#cached result


#This function creates a list of function which can cache its inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y){
        x <<- y
        v <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) v <<- inv
    getinverse <- function() v
    list(set = set, get = get, setinverse = setinverse,getinverse= getinverse)
}

# This function return inverse of a matrix. The result will be calculated or returned
# from cache.

cacheSolve <- function(x, ...) {
        
    v <- x$getinverse()
    if(!is.null(v)){
        message("getting cached data")
        return (v)
    }
    data <- x$get()
    v <- solve(data)
    x$setinverse(v)
   
    v ## Return a matrix that is the inverse of 'x'
}
