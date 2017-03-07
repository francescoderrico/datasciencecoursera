## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # starting with nothing is cached (NULL)
        # then create a matrix and empty cache
        inv <- NULL
        setMTX <- function(MTX) {
                x <<- MTX
                inv <<- NULL
        }
        
        # get the matrix related to "x" above
        getMTX <- function() {
                x
        }
        
        # set two functions and assign to setInv and getInv
        setInv <- function(inverse) {
                inv <<- inverse
        }
        
        getInv <- function() {
                inv
        }
        
        # at the end return a list of functions setted above
        list(setMTX = setMTX, getMTX = getMTX, setInv = setInv, getInv = getInv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        # the cached data is returned if is not null with relative message
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # then, 4 steps:
        # #1 get the matrix,
        # #2 compute the inverse,
        # #3 store it to the cache
        # #4 print the inverse 
        data <- x$getMTX()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
