## Put comments here that give an overall description of what your
## functions do

#' the function makeCacheMatrix is essentially a constructor
#' for a cached matrix object, which contains a list of 
#' that provide access to the environment of makeCacheMatrix
#' The environment is the cache, and  hold both the copy of the 
#' underlying matrix and the inverse. The inverse is intially set
#' to NULL and computed the first time it is needed by cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
  saved.inverse <- NULL #declares saved.inverse,as member of the makeCacheMatrix enviromnemnt
  set <- function(y) { #when called sets x to be y and resets m within the envir
    x <<- y #this is where we save the underlying matix x
    saved.inverse <<- NULL #this is where we reset the saved.inverse to NULL
  }
  get <- function() x #returns the underlying matrix object
  set.inverse <- function(inv) saved.inverse <<- inv #used by cache solve to store the inverse
  get.inverse <- function() saved.inverse # used by cache solve to retrieve  saved.inverse
  list(set = set, get = get, #getter and setters for the underlying matrix
       set.inverse = set.inverse, #
       get.inverse = get.inverse)
}


#' function cacheSolve
#' takes a cached matrix m (created via the makeCacheMatrix function)
#' and looks first to see if there is an inverse saved in the cache 
#' (where cache is simply the environment of the makeCacheMatrix function)
#' if it finds a non-NULL saved inverse, return that, otherwise
#' just simply compute the inverse, then save it in the cache and return
#' the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  saved.inverse <- x$get.inverse()
  if(!is.null(saved.inverse)) {
    message("getting cached inverse")
    return(saved.inverse)
  }
  # failed to find so we need to compute
  matrix.stored <- x$get() #get the underlying stored matrix (stored by set)
  inverse.computed <- solve(matrix.stored, ...) #do the dirty & ugly :)
  x$set.inverse(inverse.computed) # store the inverse to the cache (i.e. the makeCacheMatrix envir)
  inverse.computed #returne the freshly computed (and just cached) inverse
}
