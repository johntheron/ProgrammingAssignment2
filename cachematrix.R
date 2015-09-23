## These functions cache the inverse of a matrix to avoid repeated computation of the inverse 

## makeCacheMatrix creates a list containing functions to set the value of the matrix, 
## get the value of the matrix, set the value of the inverse and get the value of the inverse.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {  #sets the value of the matrix
        x <<- y  
        inv <<- NULL
    }
    get <- function() x  #gets the value of the matrix
    setinv <- function(inverse) inv <<- inverse  #sets the value of the inverse
    getinv <- function() inv   #gets the value of the inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix in the list created by makeCacheMatrix.
## If the inverse has already been calculated, it gets the inverse from the cache

cacheSolve <- function(x, ...) {    
    inv <- x$getinv()
    if(!is.null(inv)) {  
        message("getting cached data")
        return(inv)
    }
    data <- x$get()  #otherwise get the data
    inv <- solve(data, ...)  #and calculate the inverse
    x$setinv(inv)
    inv
}
