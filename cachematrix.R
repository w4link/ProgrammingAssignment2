makeCacheMatrix <- function(x = matrix())  {
  ##initialize to NULL
  m <- NULL
  ##set to new value
  set <- function(y) {
    x <<- y
    ##clear old value
    m <<- NULL
  }
  ##function to return the previosly stored object
  get <- function() x
  ##store new value here
  setcache <- function(solve) m <<- solve
  ##return stored value
  getcache <- function() m
  ##return list of arguments used
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}
## 
##function to store and retrieve result
cacheSolve <- function(x=matrix(), ...) {
  ## Return stored matrix
  m <- x$getcache()
  ##first check if value already stored
  ##display message while retrieving the result
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if not stored
  ##get the object
  matrix <- x$get()
  ##calculate value of inverse matrix
  m <- solve(matrix, ...)
  ##and store calculated value
  x$setcache(m)
  m
}

