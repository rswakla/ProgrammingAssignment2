## Put comments here that give an overall description of what your
## functions do
# Create functions to set and get the value of the matrix, and set and retrieve its inverse
# initialize x as type matrix and its inverse, m, as NULL
# set the value of matrix 
makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          # create functions that access (get) or mutate (set)
          set <- function(y)  {
               x <<- y
          }
          get <- function() x
          setinverse <- function(inverse) m <<- inverse
          getinverse <- function() m
          list (set = set, get = get, setinverse = setinverse, getinverse = getinverse) # list of the functions
}


## Write a short comment describing this function
## This function caches the inverse of a matrix, which can be a costly computation
# Before solving for its inverse, it checks to see if it has already been calculated
# If so, it retrieves it from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()    #first look to see if the inverse is cached
     if(!is.null(m)) {
          message("getting cached inverse")
          return (m)
     }
     data <- x$get()        # if it is not cached, get the data
     m <- solve(data, ...)  # calculate the matrix inverse
     x$setinverse(m)        # store the matrix inverse
     m
}
