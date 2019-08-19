## Put comments here that give an overall description of what your
## functions do

## This test has the goal to write a pair of functions, namely, 
## "cacheSolve" and "makeCacheMatrix" that cache the inverse of a matrix


## Write a short comment describing this function

## makeCacheMatrix is a function that creates kind of a "matrix" object that can 
## cache your inverse for the input meaning it is an invertible square matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve is a function that computes the inverse of the matrix
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
