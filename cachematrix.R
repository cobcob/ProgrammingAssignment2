## This code will provide the ability to store data within "objects" in R. These objects are created using
## functions within functions and taking advantage of different R environments.

## This function creates the object to store the cached matrix using a supplied matrix
## In addition the function returns a set of other functions to perform operations on this matrix

makeCacheMatrix <- function(x = matrix()) {
        
        I <- NULL
        
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        
        get <- function () x
        
        setInverse <- function (inverse){ 
                I <<- inverse
        }
        
        getInverse <- function () I
        
        list (set = set, get = get, getInverse = getInverse, setInverse = setInverse)

}


## This function will either invert a matrix or retrieve the inversion of the matrix if already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        
        if (!is.null(inv)) {
                message ("getting cached inverted matrix")
                return (inv)
        }
        
        matrix <- x$get()
        
        inv <- solve(matrix)
        
        x$setInverse(inv)
        
        inv
}
