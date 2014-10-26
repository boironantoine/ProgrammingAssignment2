## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## We use the cacheSolve function that enables to use the solve() function more efficiently by 
## using R's scoping rules


## We create a list containing all elements

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ### 1. set the value of the matrix

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ### 2. get the value of the matrix
    get <- function() x
    ### 3. set the value of the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    ### 4. get the value of the inverse of the matrix
    getinverse <- function() inv
    #### return our list
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## We check if the inverse has already been calculated, if so, we simply return its value
## Otherwise, it calculates it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse() #from cache
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    # If it has bot been calculated yet 
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
