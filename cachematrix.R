## makeCachematrix and cacheSolve are two functions that work in tandem to
##calculate inverse of a input matrix(which is given as an argument to 
##makeCachematrix). The actual calculation of the inverse happens in the cacheSolve
## if the calculation has not happened in the past. In case the inverse has already
## been calculated in the past then recalculation is avoided and the cacheSolve 
##function returns the value of the inverse that is already saved in the cache.
## A message mentioning the same is also printed so that the user knows when a 
## calculation is taking place and when a cached value is being returned

## This function takes the input matrix that needs to be inverted and also passes
## on the getting and setting functions to the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
set <- function(y){
    x <<- y
    }
get <- function() x
setinverse <- function(inversebeingset) inverse <<- inversebeingset
getinverse <- function() inverse
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This functions takes the matrix to be inverted from the makeCacheMatrix
## function, and also the inverted matrix if it has already been calculated 
## in the past. If the inverse is already calculated then the cached value is 
##returned and a message mentions the same. If no inverse value is stored in 
## cache then the cacheSolve function calculates it within itself and gives this
## value back to makeCacheMatrix using the $setinverse() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        print("Getting inverse from cache")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
