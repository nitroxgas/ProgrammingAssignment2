## This is my submission to the second programming assignment
## As requested, the functions shoud use solve() to invert a presumed square matrix and keep
## its value in a cache.
## For better understanding I've preserved in great part the exemple offered, this way the understanding
## in the evaluation should be better.

## makeCacheMatrix return a object with set, get, setsolve and getsolve methods. 
## Likewise it keeps the passed matrix stored at its enviroment in x;
## mat <- makeCacheMatrix(matrix(1:4,2,2))
## mat$get()   Returns the matrix
## mat$set()   Clears the cache (m) and set a new matrix to x
## setsolve()  Populate the cache with a new presumed inverted matrix
## getsolve()  Return the cache stored in its enviroment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solved) m <<- solved
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function
## cachesolve should calculate and return a inverted matrix, stored at an object created with makeCacheMatrix()
## But first it should check if something is already cached, 
## if the cache is populated, then it must return the stored value. 
## In this example, it is assumed that the matrices subjected to inversion are always square, 
## and the treatment of errors from applying solve(x) to non-invertible matrices is not the scope of the exercise

## mat <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(mat)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## from the solve() documentation, use the function below to generate a invertible matrix
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h3 <- hilbert(3) 
## h3
##          [,1]      [,2]      [,3]
##[1,] 1.0000000 0.5000000 0.3333333
##[2,] 0.5000000 0.3333333 0.2500000
##[3,] 0.3333333 0.2500000 0.2000000

## h3c <- makeCacheMatrix(h3)
## cacheSolve(h3c)
##      [,1] [,2] [,3]
## [1,]    9  -36   30
## [2,]  -36  192 -180
## [3,]   30 -180  180
