# The purpose of this assignment is to cache the inverse of
# a matrix, which requires increasingly lengthy computation
# as the matrix grows larger. So if the matrix in question
# does not change, then we should cache the inverse, so if
# we need to call on the inverse again, we do not need to
# run the entire lengthy calculation all over again.

# This function will set up user input as a matrix object
# which will be able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	x<- matrix(x, nrow = sqrt(length(x)), ncol = sqrt(length(x)))
		# This will set up a square matrix using the user's input
		# if they initialized the matrix with the data
		# Otherwise, they can define the matrix data using set()
	m <- NULL
		# m will be used to represent the cache we can call later
		# So when initializing a matrix, we must reset the cache
		# because the purpose of the cache is to save the inverse
		# only as long as the matrix we're using stays the same
	set <- function(y) { #This function sets the matrix
		r <<- sqrt(length(y))
		x <<- matrix(y, nrow = r, ncol = r)
		m <<- NULL
		# <<- assigns values to x and m in the parent environment
		# So whatever the user writes in the argument for set will
		# be set as x in makeCacheMatrix
	}
	get <- function() x
		# This retrieves the matrix x and returns it
	setinverse <- function(inverse) m <<- inverse
		# This sets whatever is fed into it as m, aka the cache
		# <<- is used to set the cache in the parent environment
		# The purpose of this is so we can call m in the 2nd
		# function to check if the inverse is saved already
	getinverse <- function() m
		# This function is how we will call the cache
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
	# This will return a list of the subfunctions defined in the
	# parent environment so we can call them as needed below
}

# This function will actually solve for the matrix's inverse
# First it will check if the inverse exists already- if it does,
# then we simply return the cache. Otherwise it computes the
# inverse and then sets that as the cache

cacheSolve <- function(x, ...) {
    # This function will return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    	# Pulls the cache, whatever it may be, from the matrix
    	# object that we created using the first function
	if(!is.null(m)){ # if m is NOT null, then run this code
		message("Retrieving Cached Data")
			return(m) #inverse is in cache, so return it
	}
	#This code runs if m IS null and therefore has NOT been set
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m) #this will save the new inverse as m
	m #return the solution
}