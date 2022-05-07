## The functions calculate the inverse of a given matrix
## trying to catch it in the cache first 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     
  i <- NULL                                       ### initialize i as NULL; it will hold value of the inverse matrix
  set <- function(y) {                            ### define the set function to assign a new value of the matrix in the env
    x <<- y
    i <<- NULL                                    ### reset the original matrix
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse  ### assigns value of i in the env
  getinverse <- function() i                     ### gets the value of i
  list(set = set, get = get,                     ### set the list to recall the function with the operator $
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()                           ### getting the inverse 
  if (!is.null(i)) {
    message("getting cached data")              ### get the message if we found the matrix in the cache
    return(i)
  }
  data <- x$get()                               ### solve the inverse matrix if it's not in the cache
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
