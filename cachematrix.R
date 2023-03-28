# Matrix calculations like inversions are computationally costly. Why not cache the results, 
# so that they dont have to recalulated.
# Before we can invert the matrix we will need to create one. Also remember that
# the matrix must be square to be invertable and the determinant must not equal zero
# (if something goes wrong , you might want to test this..difficult for big matrices)
#  M x M^-1 = Identity matrix.
#  We have several objectives 
#  A. create a matrix object for testing and proving cache technique is effective (see step three)
#  B. write a function "makeCacheMatrix()" that creates the matrix object (m) such that its inverse can be cached.
#  C. write a function "cacheSolve()" that actually calculates the inverse, where m is the matrix returned
#     by the function "makeCacheMatrix()".  we refine the function such that if
#     the M^-1 (the inverse matrix) has already been calculated and the matrix has not changed, 
#     the function retrieves the inverse from the cache without recalculation.
#  D. Testing the results: here we create a function (test), which takes 
#     an invertible matrix, calculates its inverse not once but twice using the 
#     above functions, and prints out the times it takes for both runs. 
#     The first run should take longer than the second run because the first run calculates 
#     the inverse. The second run only looks-up the result from the cache.(see step four)
makeCacheMatrix <- function(x = matrix()) {
  # x: a square invertible matrix
  # return: a list containing functions to
  #1. set x
  #2. get x
  #3. set x^-1
  #4. get X^-1
  #this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # `<<-` is used to assign a value to an object in an differnt environment
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
cacheSolve <- function(x, ...) {
  ## argument x is the output of makeCacheMatrix()
  ## return function calculates inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
# In the console run this set of instruction to generate a matrix, before running 
# the test function
set.seed(500)
r = rnorm(10000)
mat1 = matrix(r, nrow=100, ncol=100)

# This is the test function

test = function(mat){
  ## argument mat  is an invertible matrix
  
  bob = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(bob)
  dur = Sys.time() - start.time
  print(dur)
  # try it again to see if there is an effect of caching
  start.time = Sys.time()
  cacheSolve(bob)
  dur = Sys.time() - start.time
  print(dur)
}
# Next use the input "mat1" as the argument of the test function,
# when tested , the output should look something like this for a matrix that 100 by 100. 
#  Time difference of 0.001100063 secs
# getting cached data
# Time difference of  0.0001780987 secs