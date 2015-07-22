## 
## source('cachematrix.R') 
## m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2))) 
## cacheSolve(m) 
## [,1] [,2] 
## [1,]  0.5  0.0 
## [2,]  0.0  0.5 
 
## Create a function "makeCacheMatrix", which will 
##   - set the value of the matrix 
##   - get the value of the matrix 
##   - set the value of the inverse matrix 
##   - get the value of the inverse matrix 

makeCacheMatrix	<- function(x = matrix()) {

       m <- NULL
		
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
        get <- function() x
		
        setinv <- function(inv) m <<- inv
		
        getinv <- function() m
		
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}
 

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

## Calculate the inverse of the matrix and reusing cached result if it is available 
 
cacheSolve <- function(x, ...) {

        m <- x$getinv()
		
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
        data <- x$get()
		
        m <- solve(data)
		
        x$setinv(m)
		
        m
}


## Sample run:
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2))) 
## >m$get()
##     [,1] [,2]
## [1,]    2    0
## [2,]    0    2
## No cache in the first run
## > cacheSolve(m)
##       [,1] [,2]
##  [1,]  0.5  0.0
##  [2,]  0.0  0.5

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5
