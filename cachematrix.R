##programming assignment 2
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL               ##sets inverse of matrix to null
  
  ## sets matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## gets matrix
  get <- function() x           
  
  ## setting the inverse matrix
  setimatrix <- function(imatrix) 
    i <<- imatrix
  
  ## getting the inverse matrix
  getimatrix <- function() i                 
  
  ## list format
  list(set = set, get = get,
       setimatrix = setimatrix,
       getimatrix = getimatrix)
}


## computes the inverse matrix 
cacheSolve <- function(x, ...) {
  
  ## sets imatrix value
  i <- x$getimatrix() 
  
  ## returns inverse matrix if already cached
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  
  ## calculates inverse matrix if not cached
  data <- x$get()            
  i <- solve(data, ...)      ## calculates inverse
  x$setimatrix(i)             ## sets inverse of the matrix
  i                         ## returns inverse of the matrix
}
