## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This returns a list of 4 functions based on the matrix you give it
# Those functions will be able to modify and read values in the matrix

#The first thing you would do is run:    makeCacheMatrix()
# that will create an empty matrix
makeCacheMatrix <- function(x = matrix()) {

  # set the value of the inverse to Null when this matrix is initially created
  m <- NULL
  # create the function set
  # it will allow you to input a matrix, so that later you can make calculations based on it
  # the command would be x$set(data)
  # where data is a matrix
  set <- function(y) {
    # sets the matrix to whatever we input
    # if we did m1$set(matrix(1:4,2,2))   it would set the value of the matrix to that
    x <<- y
    # sets the value of the matric to null when a new matrix is input. this value can
    # be called even after this function exits
    m <<- NULL
  }
  # will return the matrix we input using x$get()
  get <- function() x
  # will set the value of the inverse and put it into the cache
  setinverse <- function(solve) m <<- solve
  # will retrieve the value of the inverse. will return Null if nothing has been calculated
  getinverse <- function() m
  
  #This is what is actually returned when you run this function
  # So you give this function a matrix x, and you can run commands such as
  # x$get() to get the matrix itself
  # x$set() to create a matrix
  # x$getinverse() to return an inverse if it was already calculated
  # x$setinverse() to put a calculated inverse into the matrix x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This will return the inverse of the matrix 'x'
cacheSolve <- function(x, ...) {
    
    # this will set the value m
    # either the inverse has been calculated and it will be a value
    # or the inverse hasnt been calculated and will return null
    m <- x$getinverse()
    
    # if we got a value for the inverse of the matrix
    # in other words, we have already calculated the cache and stored it in x
    if(!is.null(m)) {
      # print a message saying that we already cached it
      message("getting cached data")
      # return that value exit out of the function
      return(m)
    }
    # at this point, we have not found anything in the cache for the matric we have passed in
    
    # the following line retrieves the matrix
    data <- x$get()
    # we will calculate the mean of the matrix and set that value to m
    # the ... is for other values that we can pass in if needed (we dont in this case)
    m <- solve(data, ...)
    # we will put value of the inverse into the cache
    x$setinverse(m)
    # we will return the cache value to the user
    m
}
