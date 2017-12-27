# Assignment-I Peer Assignment -- Lexical Scoping
# Author: Abinash Anand G
# Created Date: 24/12/2017
#*********************************************************************************

# Initialising the value of the x matrix. Values are initialised from 1 to 9 in a 3*3 matrix.
x <- matrix(c(0,2,4,5,3,5,7,5,2),3,3)
#makeCacheMatrix-- takes x as an input argument for execution.
makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL #Initialising the matrix as an empty matrix
  # The get set functions get initialised and the values are updated.
  set <- function(y) {
    x <<- y
    mtx <<- NULL
  }
  get <- function() x
  set_matrix <- function(solve) mtx <<- mtx  #generic function solves the equation a %*% x = b for x, where b can be either a vector or a matrix.
  get_matrix <- function() mtx  # returns the stored matirx values
  list(set = set, get = get,
       set_matrix = set_matrix,
       get_matrix = get_matrix) # collects and returns all the info as a list 

}


# The functions calculates and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  # gets the cached values
  mtx <- x$get_matrix()
  #If the cached value is available it returns the values
  if(!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
  }
  #If there is no cached values then set of new calculation starts.
  data <- x$get()
  mtx <- solve(data, ...)
  x$set_matrix(mtx)
  mtx
}


#Validating the output.
x
mymatrix <-cacheSolve(makeCacheMatrix(x))
mymatrix
