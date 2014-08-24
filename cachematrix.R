## Put comments here that give an overall description of what your
## functions do

## This function creates a vector of functions which access the
## variables that exist within this functions own environment,
## similar to a class where the environment corresponds to
## private data within the class and the function instances
## correspond to members.
##
## Provides get and set for setting and getting a matrix
## Provides getInverse and setInverse for accessing said matrix's
## cached inverse matrix
##
makeCacheMatrix <- function(x = matrix())
{
    inverseMatrix <- NULL
  
    set <- function(theMatrix)
    {
        x <<- theMatrix
        inverseMatrix <<- NULL
    }
    get <- function() { x }
    
    setInverse <- function(theMatrix) { inverseMatrix <<- theMatrix }
    getInverse <- function() { inverseMatrix }
  
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of a matrix.
## If the inverse matrix has been cached, the cached matrix is returned.
## Otherwise, the inverse is computed (or fails to find a solution if none exists)
## and stores the computed matrix for future use by this function.
##  
## This function assumes the matrix argument is invertible/non-singular.
## If a singular matrix is passed in, it will fail.

cacheSolve <- function(x, ...)
{
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix))
    {
        message("getting cached data")
        return(inverseMatrix)
    }
    
    ## To find the inverse of the matrix, use solve with a default b value of an identity matrix.
    ## This will give us the inverse because solve finds the matrix that when multiplied by a
    ## (the original matrix) yields b. If b is the identity matrix, the solution must be the inverse of a.
    x$setInverse( solve(x$get(), ...) )
    x$getInverse()
}
