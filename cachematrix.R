## 
## Provides functions to take a matrix as input, cache the matrix, and
## calculate and cache the inversion of the matrix using solve().
## 
## It is presumed that the input matrix is invertable.  If not, R will
## throw and error.
## 

##
## Given an invertable matrix, return a list making the matrix cacheable.
## 
makeCacheMatrix <- function(inputMatrix = matrix()) {
    invertedMatrix <- NULL
    setMatrix <- function(y) {
        inputMatrix <<- y
        invertedMatrix <<- NULL
    }
    getMatrix <- function() inputMatrix
    setInvertedMatrix <- function(invertedMatrix) invertedMatrix <<- invertedMatrix
    getInvertedMatrix <- function() invertedMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInvertedMatrix = setInvertedMatrix,
         getInvertedMatrix = getInvertedMatrix)
}

##
## Get the inversion of a matrix from cache, or calculate and cache if needed.
## 
## Given an input value returned from makeCacheMatrix, get the inverted matrix
## from cache. If cache is empty, calculate the inversion and cache it in the 
## input object for later use.
## 
## To adhere with the SOLID principles caching should be done in the 
## makeCacheMatrix function instead, but meh...
##
cacheSolve <- function(makeCacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    invertedMatrix <- makeCacheMatrix$getInvertedMatrix()
    if(!is.null(invertedMatrix)) {
        message("getting cached data")
        return(invertedMatrix)
    }
    matrix <- makeCacheMatrix$getMatrix()
    invertedMatrix <- solve(matrix, ...)
    makeCacheMatrix$setInvertedMatrix(invertedMatrix)
    invertedMatrix
}
