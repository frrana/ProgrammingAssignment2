## Below are two functions that are used to create a special object that stores a matrix and caches it's 
## inverse of a matrix. 


## when you call makeCacheMatrix() function, and assing it's return value to a variable, then variable will
## be a list of four function : set(),get(),setmatrix(), getmatrix(). which will set the value of the vector,
## get the value of the vector, set the value of the matrix, get the value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y){
                x <<- y
                m <<- NULL
              }
               
              get <- function() x
              setmatrix <- function( matrix)  m <<- matrix
              getmatrix <- function() m
            
              list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## When you Pass the list to the cacheSolve() function which will calculate the inverse of a matrix 
## return by makeCacheMatrix(). If the inverse has already been calculated and matrix has not change, then the 
## cacheSolve() function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
   m <- x$getmatrix()

  if(!is.null(m)){
    
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  x$getmatrix()
  m
}
