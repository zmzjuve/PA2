## The function is to caching the Inverse of a Matrix

## The first function, makeCacheMatrix creates a special "matrix",
##which is really a list containing a function to:
##1.set the the matrix
##2.get the the matrix
##3.set the the inverse matrix
##4.get the the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
             M <- NULL
             set <- function(y){
                    x<<-y
                    M<<-NULL
             }
             get <- function()x
             setInverse <- function(solve) M <<- solve
             getInverse <- function() M
             list(set=set, get=get,
                        setInverse=setInverse,
                        getInverse=getInverse)

}


## The following function calculates the inverse matrix of the special 
## "matrix" created with the above function. However, it first checks to 
## see if the inverse matrix has already been calculated. If so, it gets 
## the goal from the cache and skips the computation. Otherwise, it 
## calculates the inverse matrix of the data and sets it in the 
## cache via the setInverse function.

cacheSolve <- function(x, ...) {
            M <- x$getInverse()
            if (!is.null(M)){
                 message("getting cached data")
                 return(M)
            }
            data <- x$get()
            M <- solve(data,...)
            x$setInverse(M)
            ## Return a matrix that is the inverse of 'x'
            M
        
}
