## cacheMatrix.R takes a matrix, creates it into an object for getting and setting
## purposes, and then allows for the inverse of that matrix to be calculated
## and saved into memory. If the specific inverse has already been calculated,
## it is then recovered from memory rather than recalculated.

## makeCacheMatrix takes a matrix and creates the getters and setters for it
## so it can act as an object. It returns of list with values of the getters 
## and setters.

makeCacheMatrix <- function(x = matrix()) {                     
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve utilizes makeCacheMatrix so it must be initiated first. It 
## checks to see if the inverse has already been calculated, if it has, then
## it recovers it from memory. If the inverse has not been calculated, then it
## retrieves the matrix and calcules the inverse of it, afterwards saving it into
## memory. It returns the inverse of the matrix set in the makeCacheMatrix 
## function.


cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}

