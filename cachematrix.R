## The overall goal of this pair of functions below is caching the inverse of a matrix, since matrix inversion is usally a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (in cases where the matrxi does not change during computation and all the matrices used are invertible).


## The fist function makeCacheMatrix creates a special matrix that can be stored and used in caching its inverse during the computing of the inversion. When a matrix of  interest (x as in the argument) is passed into this function, a NULL matrix is initialized (m) as result of inversion, and the matrix x is stored in get(), when the second function below is called, it can be passed along using x$get().

makeCacheMatrix <- function(x = matrix()) {
			m <- NULL
			set <- function(y) {
					x <<- y
					m <<- NULL
			}
			get <- function() x
			setinverse <- function(inverse) m <<- inverse
			getinverse <- function() m
			list(set = set, get = get,
				 setinverse = setinverse,
				 getinverse = getinverse)
}


## The following function cacheSolve calculates the inverse of the special matrix created with the function above. It first checks to see if the inverse has already been calculated (!is.null(m) == TRUE), if so, it prints out a message"getting cached data" and return the cached inverse matrix m. Otherwise, it gets the matrix using x$get() and returns it as "data", then solve the inverse of "data" by solve() function, and returns the result m in the last. 

cacheSolve <- function(x, ...) {
	 	m <- x$getinverse()
	 	if (!is.null(m)) {
	 			message("getting cached data")
	 			return(m)
	 	}
	 	data <- x$get()
	 	m <- solve (data, ...)
	 	x$setinverse(m)
	 	m   
         ## Return a matrix m that is the inverse of 'x'

}
