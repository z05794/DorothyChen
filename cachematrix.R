## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        xsinverse <- NULL
        set <- function(y) {
          x <<- y
          xsinverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) xsinverse <<- inverse
        getInverse <- function() xsinverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xsinverse <- x$getInverse()
        if (!is.null(xsinverse)) {
          message("get cached data")
          return(inv)
        }
        mat <- x$get()
        xsinverse <- solve(mat, ...)
        x$setInverse(xsinverse)
        xsinverse
}

#Test-------------------------------------------
#m <- matrix(c(4, 7, 2, 6), 2,2)
#m <- matrix(c(3,1,5,2,-3,-1,-5,2,4), 3, 3)
#xms <- makeCacheMatrix(m)
#length(xms)
#class(m)
#xms$get()
#xms$getInverse()
#cacheSolve(xms)