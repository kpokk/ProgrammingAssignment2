## Put comments here that give an overall description of what your
## functions do

## Functions makeCacheMatrix and cacheSolve are used to create a 
## special object that stores a matrix and cache's its inverse


## Write a short comment describing this function

## Function makeCacheMatrix makes list that has functions to 
## get and set a value of a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Write a short comment describing this function

## Function cacheSolve returns (and if necessary calculates)  
## inverse of a matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return (i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}