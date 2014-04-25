## The function makeCacheMatrix caches a special "matrix" and it's 
## inverse. A list of functions is returned, which are used to 
## manage the cached values of yher matrix and its inverse.

## The list of functions returned are
## set - sets the value of the matrix, and sets inverse to NULL 
## get - gets the value of the matrix  
## setinverse - sets the value of the inverse 
## getinverse - gets the value of the inverse

## makeCacheMatrix is used to cache a square invertible matrix and its inverse
        
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                xi <<- NULL             
        }
        set(x)
        get <- function() x
        setinverse <- function(inverse) xi <<- inverse
        getinverse <- function() xi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## The function cacheSolve computes the inverse of a special matrix
## assumed to be square and invertible. It checks first to see if the inverse
## has been calcualted. If so then it uses the cached value and skips the 
## calculation. 
## 
## Arguments
##
## cacheObj     A R Object. Contains the special matrix that was created 
##              using the function makeCacheMatrix
##
##   cacheSolve computes and caches the inverse of a special matrix   

cacheSolve <- function(cacheObj, ...) {
        inverse <- cacheObj$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- cacheObj$get()
        inverse <- solve(data)
        cacheObj$setinverse(inverse)
        inverse
}
