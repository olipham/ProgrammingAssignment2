## These functions calculate and cache the inverse of a matrix to avoid recalculating the inverse. 

## This function creates a special matrix, which is a list of containing functions to 
## 1) set value of matrix 
## 2) get value of matrix 
## 3) set inverse of the matrix 
## 4) get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of the matrix set in the above function. 
## If the inverse of the matrix has already been calculated, then it skips the calculation. 
## Otherwise, it calculates the inverse and caches in the setinv function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
           message("getting cached data")
           return(m)
        }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

