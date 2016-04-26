## functions used to cache the inverse of a matrix

# flow of makeCacheMatrix
# 1. set value of the matrix
# 2. get value of the matrix
# 3. set value of inverse of the matrix
# 4. get value of inverse of the matrix
# 5. creates a list containing a above functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns inverse of the matrix. 
# First checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
