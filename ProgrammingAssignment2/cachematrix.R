## The code allows its user to define a matrix and 
## store this matrix in a function. Futhermore, this 
## matrix can also have its inverse taken in stored in 
## this function. This allows users to access an 
## inverse of matrix that has already been calculated 
## to save computing time from recalculating a 
## result that has already been computed.

## This first function does four actions. First we can 
## set a value for our matrix. Then we can get the matrix
## back. It also sets the inverse of the matirx so we can
## get our inverse back by calling the getinverse function.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if the inverse
## matrix has already been found and if it has
## it will pull out the matrix from the getinverse
## function rather than compute it again. However,
## if no inverse is found, then it will find the 
## inverse of that matrix.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i        
}
