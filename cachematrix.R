## The following functions compute the inverse of a matrix and cache it, so
## that it does not have to be recomputed.
##
## The first function creates a special matrix object, which is a list of
## functions to set the value of the matrix, get the value of the matrix,
## set the value of the inverse matrix and get the value of the inverse matrix.
##
## The second function checks if the matrix object already has a cached inverse
## matrix and returns it, if it exists. If not, it will be computed and cached.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMatrix) inverse <<- inverseMatrix
        getinverse <- function() inverse
        list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}


## This function returns the inverse of the matrix object returned by 
## makeCacheMatrix. If the inverse has already been computed, this function
## will retrieve i tfrom the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse

}



# ## example
# 
# # matrix data
# M = matrix( 
# 	c(2, 4, 3, 1, 5, 7,3,1,8), 
# 	nrow=3, 
# 	ncol=3
# )
#    
# # create special matrix object
# matrixObject <- makeCacheMatrix()
# 
# # assign matrix to this object
# matrixObject$set(M)
# 
# # call inverse matrix function
# inverseMatrix <- cacheSolve(matrixObject)
# 
# # print inverse matrix
# print(inverseMatrix)
