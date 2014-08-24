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


# Unit tests for the matrix caching code above
# To run tests, call runTests()
testCacheMatrixAndSolve <- function()
{
    testMatrix <- matrix(c(2, -3, -2, 0, 0, -1, -1, 2, 0), 3, 3)
    testMatrix2 <- matrix(c(1, 4, -2, 0, 0, -1, -1, 2, 0), 3, 3)
    expectedInvertedMatrix <- matrix(c(2, -4, 3, 1, -2, 2, 0, -1, 0), 3, 3)
    
    cacheMatrix <- makeCacheMatrix(testMatrix2)
    
    if (!identical(testMatrix2, cacheMatrix$get()))
    {
        message("UNEXPECTED: Got unexpected matrix from get accessor after cache matrix initialization")
        return(FALSE)
    }
    
    # Test the set accessor by assigning a different matrix
    cacheMatrix$set(testMatrix)
    
    if (!identical(testMatrix, cacheMatrix$get()))
    {
        message("UNEXPECTED: Got unexpected matrix from get accessor after setting matrix using set accessor")
        return(FALSE)
    }
    
    # Haven't yet solved the matrix, so cache should be NULL
    if (!is.null(cacheMatrix$getInverse()))
    {
        message("UNEXPECTED: Cached matrix should be NULL because it hasn't been solved yet")
        return(FALSE)
    }
    
    # Solve the matrix and cache the value
    actual <- cacheSolve(cacheMatrix)
    if (!isTRUE(all.equal(expectedInvertedMatrix, actual)))
    {
        message("UNEXPECTED: Got unexpected inverted matrix from cacheSolve")
        message("Actual:")
        print(actual)
        message("Expected:")
        print(expectedInvertedMatrix)
        message("Original matrix:")
        print(cacheMatrix$get())
        return(FALSE)
    }
    
    ## Try again to get the cached version
    cached <- cacheSolve(cacheMatrix)
    if (!identical(cached, actual))
    {
        message("UNEXPECTED: Cached matrix should be identical to the previously returned solution")
        return(FALSE)
    }
    
    ## Assign a new matrix value and test to make sure the cache is set to NULL
    cacheMatrix$set(testMatrix2)
    if (!is.null(cacheMatrix$getInverse()))
    {
        message("UNEXPECTED: Got cached inverse matrix but should have been NULL")
        return(FALSE)
    }
    
    return(TRUE)
}

runTests <- function()
{
    message("Running testCacheMatrixAndSolve...")
    if (FALSE == testCacheMatrixAndSolve())
    {
        message("FAILED")
    }
    else
    {
        message("PASSED")
    }
    
}
