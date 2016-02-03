## Two functions
## The first creates a special "matrix" object that can cache its inverse
## The second computes the inverse of the special "matrix" returned by makeCacheMatrix

## makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse matrix
## 4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## it will however first check whether the inverse has already been calculated
## if it has, it will return the inverse from the cache without recalculating it

cacheSolve <- function(x, ...) 
{
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}