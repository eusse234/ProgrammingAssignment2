## The first function ("MakeCacheMAtrix") create a "speial" list containing a 
## matrix and caclulate its inverse using the solve function. Then the 
## "cacheSolve" function solve the inverse of the matrix set in "makeCacheMatrix"
# but first check if its inverse is already calculated into the same 
## "makeCacheMatrix" function and skip the computation.

## This function create a list containing 1) set the matrix, 2) get the matrix,
## 3) set the inverse of the matrix and 4) get the inverse of the matrix. the input
## argument will be a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = setmatrix, get = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function check if the inverse of the matrix setted in list$set
## is already been calculated in list$getinverse. If it is true the function take 
## the value from the cache, otherwise, it calculate the inverse of the data and 
## set its value in the cache via "setinverse" function.

cacheSolve <- function(x=matrix(), ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
} 

## Return a matrix that is the inverse of 'x'
