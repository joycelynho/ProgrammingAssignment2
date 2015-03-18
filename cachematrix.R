## This program enables caching of data

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above
## If the inverse has been calculated, this function will retrieve it from the cache instead
cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if (!is.null(m)) {
    message("Getting cached data")
    return (m) ## Retrieve inverse matrix from cache if it is calculated
  }
  data <- x$get()
  m <- solve(data, ...) ## To inverse the matrix
  x$setmean(m)
  m ## Return a matrix that is the inverse of 'x'
}
