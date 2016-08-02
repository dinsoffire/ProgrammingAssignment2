################################################################################
## Cache and solve matrix:                                                    ##
################################################################################
## calculate inverse of a matrix and cache the result.                        ##
## if cache exists do not calculate again, instead take inverse from cache    ##
################################################################################

## makeCacheMatrix
##################
# Returns a list of the necessary functions to get inverse of a supplied matrix
# In the scope of the function, x is the matrix to be inverted
# In the scope of the function, inverse will be the inverse of x


makeCacheMatrix <- function(x = matrix()) {
    inverse     <- NULL              # empty matrix to contain inverse
    
    # function to supply a new matrix to be inverted in parent environment
    setMatrix   <- function(newData){ 
        inverse <<- NULL             # reset inverse
        x       <<- newData          # change source matrix to new data
    }
    
    # output matrix x
    getMatrix   <- function() x      
    
    # assign a matrix to inverse in parent environment
    setInverse  <- function(solved) {
        inverse <<- solved 
    } 
    
    # output inverse
    getInverse  <- function() inverse 
    
    # output of function is list of functions which belong to 
    # matrix x's current value
    list(    setMatrix  = setMatrix
           , getMatrix  = getMatrix     
           , setInverse = setInverse
           , getInverse = getInverse
    )
}

## cacheSolve
#############
# Returns the inverse of a matrix to which makeCacheMatrix has been applied
# if the inverse has been cached, output cache
# if inverse has not been cached, output calculated inverse

# supply output of makeCacheMatrix, optional input is new matrix for set function.
cacheSolve  <- function(x, ...) { 
    inverse <- x$getInverse()   # get inverse from x if exists
    if(length(inverse) != 0){   # if inverse was cached
        message("A cache exists of inverse. Output cache.")
        return(inverse)         # return cache, end function
    } 
    matr    <- x$getMatrix()    # get source matrix
    inverse <- solve(matr)      # calculate inverse
    x$setInverse(inverse)       # cache inverse
    inverse                     # return inverse
}
