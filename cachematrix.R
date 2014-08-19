## Put comments here that give an overall description of what your
## functions do
##----------------------------------------------------------------------
## The makeCahceMatrix function creates the closure functions
## set and get to set and get the matrix and the setinv and getinv
## functions to set the inverse once it is computed and get the 
## computed inverse. It takes a matrix as a parameter
## set(): This function takes in a new matrix as parameter and
##        sets the enclosed matrix variable m with that value.
##        It also invalidates the m_inv in the parent environment
##        so that when inverse is asked for the next time we will be
##        forced to compute it
## get() : Returns the current stored matrix in m
## setinv(): Sets the inverse value of the matrix
## getinv(): Gets the cached inverse value of matrix stored in m_inv
##
## The function then returns a list each entry in the list is a vector
## with one function where the name of the column is the same name as
## the function which makes it easy and intuitive to invoke the function
## e.g. m$getinv()
##----------------------------------------------------------------------
makeCacheMatrix <- function(m = matrix()) {
  m_inv <- NULL
  # Set the internal matrix variable m with the passed in matrix y
  # Reset the m_inv variable to NULL to force it to be recomputed
  set <- function(y) {
    m <<- y
    m_inv <<- NULL
  }
  
  # Return the internally stored matrix m
  get <- function() m
  
  # Set the inverse of the matrix to the passed in value
  # m_inv is the free variable and we use <<- to pull it from 
  # th parent environment and set it 
  setinv <- function(mat_inv) m_inv <<- mat_inv
  
  # Function to return the cached matrix inverse
  getinv <- function() m_inv
  
  # This creates and returns a list with each function as the list member 
  # with identical column names making it intuitive to call the function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##----------------------------------------------------------------------
## The function is a cached version of the normal call to solve a matrix
## with solve(m).
## Function goes through the following steps - 
## 1. Use getinv() on the passed in makeCacheMatrix matrix
## 2. If this the first time getinv() is called on this, it will 
##    return a NULL. If it is not null then print a message to indicate
##    we are accessing the cached inverse value and return
## 3. If inverse value is NULL then inverse hasn't been computed. 
##    Compute the inverse using the standard solve function. Any parameter
##    in addtion to makeCacheMatrix that are passed into cacheSolve are
##    passed on into solve function here
## 4. The inverse computed is saved in makeCacheMatrix through call to
##    setinv()
## 5. Inverse value is returned
##----------------------------------------------------------------------
cacheSolve <- function(m, ...) {
  ## Get the inverse of 'm'
  m_inv <- m$getinv()
  
  # If the inverse returned is not null then we have used
  # the cached inverse value and we are done
  if(!is.null(m_inv)) {
    # Getting cahced inverse
    message("getting cached data")    
    # Return the cached inverse
    return(m_inv)
  }
  
  # If the inverse returned was null then we need to compute the inverse
  # the first time and cache it
  data <- m$get()
  m_inv <- solve(data, ...)
  # Cache the inverse
  m$setinv(m_inv)
  # Return the computed inverse
  m_inv
}
