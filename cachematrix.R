
## The following functions find the inverse of a matrix. Two functions are used to do this.
## makeCacheMatrix creates a special matrix object, and then cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead find it in the cache and return it 
## (and not calculate it again).

## makeCacheMatrix creates a special matrix object, with input x as a matrix. It returns an object with 
## 4 elements in its structure: set, get, setinverse, and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) m <<-inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function. which when used with makeCacheMatrix, can calculate the inverse of a matrix with
## the help of the special matrix object created in makeCacheMatrix. If the matrix inverse has been computed
## previously, a message will be returned and the value will be obtained from the cache. Otherwise, the matrix
## will be inverted

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
