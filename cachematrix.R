## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. 
## So below is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        
        ## set the matrix
        setmatrix <- function( matrix ) {
                x <<- matrix
                i <<- NULL
        }
        
        ## get the matrix
        getmatrix <- function() {
                x
        }
        
        ## set the inverse of the matrix
        setinverse <- function(inverse) {
                i <<- inverse
        }
        
        ## get the inverse of the matrix
        getinverse <- function() {
                i
        }
        
        ## Return a list of the methods
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        invx <- x$getinverse()
        
        # if matrix inverse has been already computed then it return cached matrix inverse
        if (!is.null(invx)) {
                message("inverse is cached")
                return(invx)
        }
        
        # if matrix inverse has not been computed then compute inverse of matrix 
        m <- x$getmatrix()
        invx <- solve(m) %*% m
        
        # cache inverse of matrix
        x$setinverse(invx)
        
        # return inverse of matrix
        return(invx)
}

