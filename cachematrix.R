# OVERALL DESCRIPTION
# -------------------
# makeCacheMatrix and cacheSolve enable calculation of the inverse of a matrix,
# without having to re-calculate the inverse redundantly.

# EXAMPLE USAGE
# -------------
# > m = makeCacheMatrix(matrix(1:4,2,2))    # initialise the "special matrix"
# > m$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# 
# > m$getInverse()      # this shows that the inverse has not been calculated yet
# NULL
#
# > cacheSolve(m)       # first time calculating the inverse
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# > cacheSolve(m)       # second time calculating the inverse...
# getting cached data   # <-- this message verifies that inverse is retrieved from cache
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# > m$get() %*% cacheSolve(m)   # multiplying matrix by its inverse yields 
# getting cached data           # an identity matrix (as expected)
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1




# Makes a "special matrix," which is a list of four functions, that...
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix's inverse
# 4. get the vlaue of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) 
{   mtxInverse <- NULL  # variable to store the cached inverse matrix
    
    set <- function(m)
    {   x <<- m
        mtxInverse <<- NULL
    }
    
    get <- function()
    {   return(x)
    }
    
    setInverse <- function(i)
    {   mtxInverse <<- i
    }
    
    getInverse <- function()
    {   return(mtxInverse)
    }
    
    return(list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
        ))
}


# Return the inverse matrix of "special matrix" x.
# This function will check to see if the inverse matrix is already available,
# if not, it will calculate and cache it, 
# so that the next call can simply retrieve it from cache (as opposed to having to re-calculate).
cacheSolve <- function(x, ...)
{   i = x$getInverse()
    
    if (!is.null(i))
    {   message("getting cached data")
        return(i)
    }
    
    x$setInverse(solve(x$get(), ...))
    return(x$getInverse())
}
