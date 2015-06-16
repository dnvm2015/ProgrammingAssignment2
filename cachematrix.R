## The two functions below create a special object that stores a 
## numeric matrix and caches its inverse.


## The function "makeCacheMatrix" creates a special matrix. More precisely, it 
## stores a list of four functions which allow one 
## to set the value of a matrix, to get the value of the matrix, to set the 
## value of the inverse matrix, and to get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The function "cacheSolve" calculates the inverse of the special matrix 
## created with "makeCacheMatrix". It first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse 
## and sets the value of the inverse in the cache via the "setinv" function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
