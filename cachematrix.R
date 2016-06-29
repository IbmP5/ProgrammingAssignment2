## Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it 
##repeatedly (there are also alternatives to matrix inversion that 
##we will not discuss here). 

#Our assignment is to write a pair of functions that 
#creates a special "matrix" object that can cache its inverse.

##This first function will:
##       -Set the value of the matrix
##       -Get the value of the matrix
##       -Set the value of inverse of the matrix
##       -Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        MatInv <- NULL
        set <- function(y) {
                x <<- y
                MatInv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) MatInv <<- inverse
        getInv <- function() MatInv
        list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MatInv <- x$getInv()
        if (!is.null(MatInv)) {
                message("Getting the cached data")
                return(MatInv)
        }
        mat <- x$get()
        MatInv <- solve(mat, ...)
        x$setInv(MatInv)
        MatInv
}
