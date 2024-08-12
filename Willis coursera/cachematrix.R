## a pair of functions to cache the inverse of a matrix

## makeCacheMatrix() is a function that creates a matrix object that can cache its inverse

#the first step is to initialize objects x and m; specify x as an empty matrix and set n to NULL
makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  
#define set function, which (1) uses the super-assignment operator to assign 
#the input argument y to the object x in the parent environment and (2) resets n to NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  #define the function that retrieves the matrix x
  get <- function() x
  #define the function that sets the value for the inverse matrix n
  setinverse <- function(solve) n <<- solve
  #define the function that retrieves the value n
  getinverse <- function() n
  #create a list of defined objects and return it to the parent environment so they
  #can be called later with $
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##cacheSolve() is a function that computes the inverse of the special "matrix" 
#returned by the function makeCacheMatrix 

#begin by initializing x as a formal argument and allowing additional arguments 
#to be passed to the function with ...
cacheSolve <- function(x, ...) {
  
  #first try to retrieve an inverse value from the object passed in 
  #by calling the getinverse function
        n <- x$getinverse()
        #check if the result is null, if not return message
        if(!is.null(n)) {
          message("getting cached data")
          return(n)
        }
        #gets the matrix from the input object and solve for the inverse 
        data <- x$get()
        n <- solve(data, ...)
        #use the setinverse() function on the input object 
        x$setinverse(n)
        #return n to the parent environment
        n
}


