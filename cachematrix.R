## two functions that define a structure capable of holding a cached inverse matrix
## and use that structure to calculate and cache the inverse of a matrix

## makeCacheMatrix - this function takes a matrix and returns a list containing getter and setter functions
## for the matrix itself as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_matrix <<- inverse
        getinverse <- function() inv_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve - takes the list created from 'makeCacheMatrix' and returns the cached inverse
## if it exists, otherwise it calculates the inverse, caches it, and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
