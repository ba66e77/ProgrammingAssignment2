## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(inputMatrix = matrix()) {
    invertedMatrix <- NULL
    setMatrix <- function(y) {
        inputMatrix <<- y
        invertedMatrix <<- NULL
    }
    getMatrix <- function() inputMatrix
    setInvertedMatrix <- function(mean) invertedMatrix <<- mean
    getInvertedMatrix <- function() invertedMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInvertedMatrix = setInvertedMatrix,
         getInvertedMatrix = getInvertedMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(makeCacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    invertedMatrix <- makeCacheMatrix$getInvertedMatrix()
    if(!is.null(invertedMatrix)) {
        message("getting cached data")
        return(invertedMatrix)
    }
    data <- makeCacheMatrix$getInvertedMatrix()
    invertedMatrix <- solve(data, ...)
    makeCacheMatrix$setInvertedMatrix(invertedMatrix)
    invertedMatrix
}
