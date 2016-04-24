## This R file defines two functions that can be used for making a special 
## matrix object and return (and cache) matrix's inverse matrix. If the matrix
## is changed then its inverse is cleared from cache so that in future wrong
## cached values are not returned.
## This R file defines following two functions
## 1. makeCacheMatrix() : Create a special matrix object 
## 2. cacheSolve() : Cache and return inverse of matrix associated with special
##                   matrix. 


## makeCacheMatrix: This function creates a special matrix object from the 
## matrix that is passed as argument. That object provides functions to set and
## get the matrix and setInverse and getInverse functions to set and get inverse
## of the matrix. 

makeCacheMatrix <- function(inputMatrix = matrix()) {
    inverseMatrix <- NULL
    set <- function(setInputMatrix) {
        inputMatrix <<- setInputMatrix
        # set inverse of matrix to NULL is associated matrix is changed
        # by calling set function.
        inverseMatrix <<- NULL
    }

    get <- function() {
        return (inputMatrix)
    }

    setInverse <- function(setInverseMatrix) {
        inverseMatrix <<- setInverseMatrix
    }

    getInverse <- function() {
        return (inverseMatrix)
    }

    return (list (set = set,
                  get = get,
                  setInverse = setInverse,
                  getInverse = getInverse
                 )
            )
}

## cacheSolve: This function calculates inverse of a matrix associated with the
## special matrix object. The special matrix object is created by 
## makeCacheMatrix function. The cacheSolve function also cache the inverse 
## matrix so that it doesn't need to calculate the inverse of the same matrix
## multiple times. 

cacheSolve <- function(specialMatrixObject) {
    ## Return a matrix that is the inverse of 'specialMatrixObject'
    inverseMatrix = specialMatrixObject$getInverse()
    if (!is.null(inverseMatrix)) {
        message("Returning cached inverse matrix")
        return (inverseMatrix)
    }
    else {
        inputMatrix <- specialMatrixObject$get()
        inverseMatrix <- solve(inputMatrix)
        specialMatrixObject$setInverse(inverseMatrix)
        return (inverseMatrix)
    }
}
