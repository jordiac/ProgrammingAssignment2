## Matrix inversion is costly in coputation so
## there are some benefits to "caching" the inverse
## rather than calculate it repeatedly


## The following 2 functions creates a special object that stores 
## a matrix and then calculate its inverse


#-----------------------------------------------------------------------------
#This function creates a "special" matrix that can cache its inverse
#If special matrix inverse has not been calculated, NULL value will be returned

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
##This function computes the inverse of the special matrix 
#(defined with makeCacheMatrix), If inverse has already
#been calculated, cacheSolve retrieves the inverse matrix fom the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        #adding condition to know if the inverse has already been computed
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data,...)
        x$setInverse(inver)
        inver
}
#------------------------------------------------------------------------