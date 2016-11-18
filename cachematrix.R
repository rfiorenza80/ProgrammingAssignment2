## These functions will create a special matrix object and cache it's inverse.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function () i
        
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## The following function calculates the mean of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        
        i <- solve(data)
        x$setinverse(i)
        i
}


## Example Output
##> x = rbind(c(1:2), c(4:3))
##> i = makeCacheMatrix(x)
##> i$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    4    3
##> cacheSolve(i)
##      [,1] [,2]
## [1,] -0.6  0.4
## [2,]  0.8 -0.2
