## This function stores a matrix and its inverse in 
## memory.  It also provides set/get access "methods" (functions) to 
## access and update the matrix and inverse variables
makeCacheMatrix <- function(mtrx = matrix()) {
    
    ## Init varibale that will hold inverse
    cachedMatrixInv = NULL
    
    ##=============== SET FUNCTION
    ## Sets the matrix variable "mtrx" to a new matrix value "newMtrx"
    fnSet <- function(newMtrx){
        
        ## Reset matrix variable
        mtrx <<- newMtrx
        
        ## Since we're reseting matrix variable, re-init cache variable
        cachedMatrixInv <<- NULL
        
    }
    
    ##=============== GET FUNCTION
    ## Return matrix variable
    fnGet <- function() mtrx
    
    
    
    ##=============== SETINV FUNCTION
    ## Set inverse of matrix
    fnSetInv <- function(inv) cachedMatrixInv <<- inv
    
    
    
    ##=============== GETINV FUNCTION
    ## Return cached marix inverse
    fnGetInv <- function() cachedMatrixInv
    
    
    
    ##=============== RETURN LIST OF FUNCTIONS
    list(set = fnSet, 
         get = fnGet,
         setInv = fnSetInv,
         getInv = fnGetInv)
    
}


## This function accepts a "makeCacheMatrix" object
## and returns the inverse matrix value stored in that object.
## If the inverse was not cached, then it calculates it, caches
## it and then returns it.  Otherwise, it just returns the 
## cached value.
cacheSolve <- function(matrixObj, ...) {
    
    
    ## Get Matrix Inverse
    mtrxInv <- matrixObj$getInv()
    
    ## Was something found?
    if(!is.null(mtrxInv)) {
        
        ## Return matrix and exit function
        message("getting cached data")
        return(mtrxInv)
    }
    
    ## If we got to this point, then matrix inverse was NOT cached.  
    ## Need to calculate inverse.  
    
    ## Cache matrix variable
    matrixObj$setInv(
                      ## Calculate inverse of matrix variable
                      solve(
                                ## Get matrix variable
                                matrixObj$get()
                           )
                    )
    
    ## Return inverse
    mtrxInv <- matrixObj$getInv()
    mtrxInv

}
