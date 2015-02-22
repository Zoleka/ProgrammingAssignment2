##This part of the function calculates the invest of a matrix and stores it elsewhere (called caching it) for later use


makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ##This part applies when the inverse of the matrix is called. It checks to see if that data has already been cached and if so uses the cached valued if not it goes ahead and calculates the inverse anyway
  #Changing it to get the inverse matric not the mean using the solve function
  setinvert <- function(solve) m <<- mean
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}
       


## These are important because takes like calculating the inverse of a matrix take a lot of processing time. Doing it repeatedly can make your code very slow. Therefore having a cached value can help speed up run times

cacheSolve <- function(x, ...) {
  m <- x$getinvert() #Changed to reflect above changes
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #Mean changed to solve as we are inverting now
  x$getinvert(m) #Changed to reflect above changes
  m
}
