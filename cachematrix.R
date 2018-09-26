## This program is made to accelerate repeated matrix inverse calculations
## by making a cache matrix who's inverse object is saved at first calculation
## and then reused every next call. To do so two functions were made:
## * makeCacheMatrix
## * cacheSolve

## The makeCacheMatrix takes a matrix as input. An inverse object is then
## created. Both the matrix and its inverse are refered to through get and set
## functions. A list containing the get and set functions for the two matrices
## is returned
makeCacheMatrix <- function(A = matrix()) {
    A_inv <- NULL
    set <-function(y){
        A <<- y
        A_inv <<- NULL
    }
    
    get <- function() A
    set_inverse <- function(inverse) A_inv <<- inverse
    get_inverse <- function() A_inv
    
    list(
      set = set, get = get,
      set_inverse = set_inverse,
      get_inverse = get_inverse
      )
    
}


## The cacheSolve takes as input the list from the makeCacheMatrix to:
## 1) Check if the inverse is already calculated, if so, reuse
## 2) If the inverse was not calculated then the matrix is taken from the get
##    function and the inverse is calculated. The calculated inverse is then
##    stored through the set function
##  The output of the function is the inverse matrix
## functions do
cacheSolve <- function(A, ...) {
    inv <- A$get_inverse()
    if (!is.null(inv)){
        message('Getting the cached inverse')
        return(inv)
    }
        
    data <- A$get()
    inv <- solve(data, ...)
    A$set_inverse(inv)
    return(inv)
}
