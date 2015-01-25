## The functions written below try to cache the inverse of a matrix, rather than compute it repeatedly,
## which increases the speed of the process.

## The makeCacheMatrix function returns a list, in which each element is also a function.
## This function serves the purpose of caching the inverse of a matrix.

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


## The cacheSovle function returns the inverse of the matrix set above. If the inverse has already
## been calculated, then it will be directly got from the cache. Otherwise the cacheSolve function
## will compute the inverse and save it in the cache.

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

## Here is an example of computing the inverse of matrix(1:4, 2, 2)
## Firstly, we set the matrix:
##     CacheList <- makeCacheMatrix()
##     CacheList$set(matrix(1:4, 2, 2))
## Then you can calculate the inverse by using the following function:
##     cacheSolve(CacheList)
## R returns the following result:
##           [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
## If you run the last function again, you would get:
##     getting cached data
##           [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
