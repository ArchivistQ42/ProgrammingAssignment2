## title:   "R Programming (Coursera): Programming Assignment 2"
## author:  "ArchivistQ42"
## date:    "16 Sept 2020"

## Collectively, these functions create and operate on special lists objects
## which can store a matrix and a cached copy of its inverse (after the first
## time has been calculated by cacheSolve

## parameters: 
  ## mat : an invertable square matrix
## return: a list of functions 
  ## set(x)     : sets the matrix to x
  ## get()      : returns the matrix
  ## setInv(x)  : sets the stored inverse to x
  ## getInv()   : returns the stored inverse matrix
makeCacheMatrix <- function(mat = matrix()) 
{
  inv <- NULL
  set <- function(newmat)
  {
    mat <<- newmat
    inv <<- NULL
  }
  
  get <- function() { mat }
  setInv <- function(newinv) { inv <<- newinv }
  getInv <- function() { inv }
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## parameters:
  ## spMat : specially formatted list returned from makeCacheMatrix(mat)
  ## ...   : any other parameters to be used in the solve function
## return: matrix inverse of matrix passed to makeCacheMatrix or set
cacheSolve <- function(spMat, ...) 
{
  i <- spMat$getInv()
  if(!is.null(i))
  {
    print("Cached inverse: ")
    return(i)
  }
  i <- solve(spMat$get(), ...)
  spMat$setInv(i)
  return(i)
}
