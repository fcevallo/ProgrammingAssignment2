## Description:
## This function will cache the inverse of a matrix.
## If the contents of the matrix are not changing, 
## the inverse of the matrix will be looked up in the cache
## rather than compute it repeatedly.
##
## Process:
## Create two functions:
##      1. makeCashMatrix
##              a. set the value of the matrix
##              b. get the value of the matrix
##              c. set the value of the inverse
##              d. get the value of the inverse
##      2. cacheSolve
##              a. change the matrix being cached
##              b. inverse matrix - solve(x)
##                      solve (x) returns the inverse of a square matrix
##              c. return matrix being cached
## 
##
## 1: Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cachedInverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                cachedInverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedInverseMatrix <<- inverse
        getInverse <- function() cachedInverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 2: Computes the inverse of the special "matrix" returned by 
## makeCacedNatrix above.  If the inverse has already been 
## calculated (and the matrix has not changed),
## then the cacheSolve retrieved the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
