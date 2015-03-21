## This program consists of two functions, makeCacheMatrix and CacheSolve, which works together to enable caching of data
## Description of each function is included below

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL ## initialize the inverse variable to be null                                                    
        set <- function(y) { ## function which takes in user i/p of the data matrix                                                
                x <<- y ## assigns user i/p to x in the parent environment
                inverse <<- NULL ## resets inverse variable to be null
        }
        get <- function() { 
                x ## simply returns the data vector
        }
        setinv <- function(inv) {
                inverse <<- inv ##set value of inverse in parent environment with user i/p of inv
        }
        getinv <- function() { 
                inverse ## simply returns the inverse variable
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv) ## returns a list with the 4 functions
}


## This function computes the inverse of the "special matrix" returned by makeCacheMatrix above
## If the inverse has been calculated, this function will retrieve it from the cache instead
cacheSolve <- function(x, ...) {
        inverse <- x$getinv() 
        if (!is.null(inverse)) { ## if inverse was calculated before, we retrieve the inverse matrix from cache
                message("Getting cached matrix")
                return (inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...) ## To inverse the matrix
        x$setinv(inverse) ## set value of inverse to be the one we just calculated above
        inverse ## Return a matrix that is the inverse of 'x'
}
