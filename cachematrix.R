#-------------------------------------------------------------------------------------------
# This function creates a special "matrix" object that can cache its inverse.
#-------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        
        getinverse <- function() m
        
        # Function returns a list
        list(set=set , get=get, setinverse=setinverse, getinverse=getinverse)
}

#-------------------------------------------------------------------------------------------
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
#-------------------------------------------------------------------------------------------

cacheSolve <- function (x,...) {
        m <- x$getinverse()
        
        # If m is not null show cached version of inverse matrix
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get() #get matrix
        
        # Try to create inverse of matrix
        result <- tryCatch({
                m <- solve(data, ...) #Create inverse of matrix
        }, warning=function(war) {
                print(paste("Warning: ",war))
        }, error=function(err) {
                        print(paste("Error occured: ",err))
                }
        )
        
        x$setinverse(m)
        
        #Function returns inverse of matrix
        m
}
