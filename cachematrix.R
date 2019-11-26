## The following example utilizes lexicopic searching in R and optimizes lengthy maths functions by caching existing computations and skips the work.

## First, let's build a function that will return the inverse of any inversible matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i<<-solve
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## Now we write a function that first retrieve the archived cache if the matrix has already been inversed. Otherwise run the solve() function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
        	message ("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
