## makeCacheMatrix: a function that creates a special "matrix" object
##that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache.
##******************************************************************##

## x is a special matrix
## that returns a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
## where the list will be used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The cacheSolve() function utilizes the list generated
## in the makeCacheMatrix() function and calculate the
## inverse of the matrix data. If this was previously
## calculated, the inverse is retrieved from the cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()

        ## If the inverse was previously calculated
        ## get it from the cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

        ## If the above is false, caculate the inverse
    mat.data <- x$get()
    inv <- solve(mat.data, ...)
    
        ## Sets the cached inverse value via setinverse function
    x$setinverse(inv)
    inv
}
