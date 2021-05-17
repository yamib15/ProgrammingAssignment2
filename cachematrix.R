## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		
		#initialising the key for Caching
		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		
		#Creating getters and setters
		get <-  function() x
		setSolve <- function(solve) m <<- solve
		getSolve <- function() m
		
		# Caching the values in a list to store for future use
		list(set = set, get = get, 
		setSolve = setSolve ,
		getSolve = getSolve )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
		m <- x$getSolve()
		
		#Checks if there is Cached value for m
		if(!is.null(m)) {
			message("getting cached data")
            return(m)
		}
		
		#If there isn't Cached value of m
		#Fetching the key
		data <- x$get()
		#Calculating the inverse of the matrix
		m <- solve(data)
		#Storing the result for Caching
		x$setSolve(m)
		m

}
