## This program is leveraging R scoping rule to fetch the inverse of a matrix from
## its cache rather than recalculating, provided the matrix has not changed 


## The below function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse.matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse.matrix <<- inverse
    getinverse <- function() inverse.matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## The below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse.matrix <- x$getinverse()
    if(!is.null(inverse.matrix)) {
        message("getting cached data")
        return(inverse.matrix)
    }
    mat <- x$get()
    inverse.matrix <- solve(mat, ...)
    x$setinverse( inverse.matrix)
    inverse.matrix
}
