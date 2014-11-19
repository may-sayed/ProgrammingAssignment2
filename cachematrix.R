##This code caches the inverse of matrix, if the matrix doesn't have its inverse cached
##it calculate it and store it in cache for future use


##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #contain the inverse of matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse #set inverse of matrix
        getinverse <- function() inv #return inverse of matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() ##call function getmean to return mean if available
        if(!is.null(inv)) { ##check if mean is not null then the 
                message("getting cached data")
                return(inv)
        }
        data <- x$get() # return the matrix
        inv <- solve(data) #return Inverse of A where A is a square matrix.
        x$setinverse(inv) #set the inverse of matrix to calculate inverse
        inv # return the matrix inverse
}


