## Put comments here that give an overall description of what your
## functions do

## This function creates a list object to store an input matrix (x) and cache its inverse (invMat) after its calculated.
## It provides 4 sub-functions to get/set the matrix and its inverse.
## Input:
##         x  an invertable matrix 
## Return:
##         a list object

makeCacheMatrix <- function(x = matrix()) 
{
  ## To store inverse of x initial value is NULL, 
  ## means not calculated yet
  invMat <- NULL
  
  ## Store input matrix into the internal variable
  ## Input:
  ##        y an invertable matrix
  ## Return:
  ##        none
  ##
  set <- function(y)
  {
    x <<- y
    ## Inverse is not calculated yet
    invMat <<- NULL
  }
  
  ## Return the matrix
  ## Input:
  ##        none
  ## Return:
  ##        the matrix stored in x
  ##
  get <- function() x
  
  ## Store input matrix invese into invMat
  ## Input:
  ##      inv inverse the matrix stored in x
  ## Return:
  ##        none
  ##
  setInv <- function(inv) invMat <<- inv
  
  ## Return stored inverse of input matrix
  ## Input:
  ##        none
  ## Return:
  ##        cachached inverse of the matrix if previously calculated othervise NULL
  ##
  getInv <- function() invMat
  
  ## Implemented functions list
  list(set = set, get = get,
       setinv = setInv,
       getinv = getInv)
}

## This is a cached version of the standard solve() function.
##
## It calculates and returns the inverse of the input matrix only at the first usage.
## In all consecutive call it returns with the cashed version of invese
## Every call it checks if the inverse cashed and the cashed inverse match the input matrix
## If either of these conditions fail then recalculte the inverse
##
## This function uses same parameters as the original solve except the first
## parameter. It should be a matrix created by the makeChacheMatrix() function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'

  ## A simple error checking. If the first argument is accindentialy an atomic matrix without the 
  ## necessary built in functions (get, set, getinv, setinv), then reports an error message and stops
  ##
  ## (It would be better, safer and reliable to create a new class for cacheMatrix and check it with class().)
  ## (Othervise if the input is matrix then it can be considerable to fall back to call original, expensive solve() function.)
  ##
  if (class(x)[1] == "matrix")
  {
    stop("This cacheSolve() function works with a matrix created with makeCacheMatrix() function only!")
  }
  
  ## Get the input matrix. It is need for check unchanged state and/or to calculate inverse
  mat <- x$get()
  
  ## Get cached inverse
  invMat <- x$getinv()

  ## Check there is valid cached inverse
  if (! is.null(invMat)) 
  {
    ## Yes, there is an inverse, but should check the matrix didn't change.
    ## The production of the matrix (x) and its inverse (inv(x)) should give an identity (E) matrix.
    ## E == x*inv(x)
    ##
    ## (diag(dim(mat[1])) create an identity matrix same size as mat.)
    
    if (identical( (mat %*% invMat), diag(dim(mat)[1]))) 
    {
      ## Yes, the input matrix didn't changed
      message("getting cached inverse matrix")
      
      ## Return the cached version of inverse
      return(invMat)
    }
  }
  
  ## There isn't cached inverse value (invMat was NULL) or the product of the matrix and the cached 
  ## inverse didn't produce an identity matrix
  ##
  ## We should calculate the inverse 
  invMat <- solve(mat, ...)
  
  ## store it now.
  x$setinv(invMat)
  
  ## Return the current inverse
  invMat
}