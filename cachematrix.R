## These two functions handle creating the inverse of a matrix and store the inverse in memory.
##  Matrix inversion that has already been completed with be retrieved from memory for efficiency. 

#sample input
#> mdat <- matrix(c(1,0,4,1,3,4,4,1,0), nrow = 3, ncol = 3, byrow = TRUE)
#> env <- makeCacheMatrix()
#> env$set(mdat)
#> ans <- cacheSolve(env)
#sample output
# > ans
# [,1]        [,2]    [,3]
# [1,]  0.08333333 -0.08333333  0.2500
# [2,] -0.33333333  0.33333333  0.0000
# [3,]  0.22916667  0.02083333 -0.0625

## makeCacheMatrix function creates a special "matrix" object that can cache the
## inverse of a matrix.  Function wll setup environment for storing the inverse 
## in memory for efficiency incase there are multiple calls for the same matrix.
makeCacheMatrix <- function(x = matrix()) {
  ## create a special "matrix" object that can cache its inverse
  m <- NULL
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) m <<-inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}
## cacheSolve function computes the inverse of a matrix using the Solve() function. 
## Function makeCacheMatrix is used to setup environment for this function.  If inverse
## has been calculated (and matrix not changed), the cacheSolve function will retrieve 
## the inverse from cache and the message 'getting cached data' is displayed. 

cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'      
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    }
  mdata <- x$get()
  m <- solve(mdata, ...)
  x$setinverse(m)
  m
}
