## SUMMARY
## These two functions below allow you to take a square invertible matrix
## and store its inverse within its cache to allow you to use the inverse
## of the matrix in calculations without having to constantly compute the
## inverse on the fly.   Some sample uses/calls are as follows:

## 2x2 matrix
## myMatrix <- makeCacheMatrix(matrix(1:4,2,2))
## myMatrix$get()
## myMatrix$getinverse() ## note, nothing returned as inverse not calculated yet
## cacheSolve(myMatrix)
## cacheSolve(myMatrix)  ## note, message indicating retrieving inverse from cache
## myMatrix$getinverse() ## note, cached results now available

## 3x3 matrix
## myMatrix <- makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0),3,3))
## myMatrix$get()
## myMatrix$getinverse() ## note, nothing returned as inverse not calculated yet
## cacheSolve(myMatrix)
## cacheSolve(myMatrix)  ## note, message indicating retrieving inverse from cache
## myMatrix$getinverse() ## note, cached results now available


## makeCacheSolve
## this function takes as an argument a matrix that is assumed to be a
## square invertible matrix.   it creates a group of functions that can be
## used to get/set the original matrix data as well as the inverse of the
## original matrix, if it exists

makeCacheMatrix <- function(x = matrix()) {

        # i is the inverse of the matrix x -- init is value
        i <- NULL

        # define "methods" available to makeCacheMatrix object
        # set - inits matrix value X and resets the cached inverse matrix i to null
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        # get returns original matrix
        get <- function() x

        # setinverse assigns inverse value to i
        setinverse <- function(inverse) i <<- inverse

        # "getinverse" returns the inverse value i
        getinverse <- function() i

        # return an object of "methods"
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve
## Returns a matrix that is the inverse of the matrix x from
## cache if it has already been calculated, else it will calculate the
## inverse on the fly, cache the result, and return the result to calling
## environment.   The matrix is assumed to be a square invertible matrix,
## else this function will generate an error message if an inverse is not
## calculable

cacheSolve <- function(x, ...) {

        # get inverse from cache, if it exists
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        # calculate the inverse on the fly, and add it to cache
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
