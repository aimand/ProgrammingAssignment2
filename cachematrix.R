## The function behaves like a class as it has four members:
## 1. a 'set' function
## 	This function sets a matrix to object created by makeCacheMatrix function
## 2. a 'get' function
## 	This function gets the matrix 
## 3. a 'setInv function
## 	This function sets the Inversed Matrix
## 4. a 'getInv function
## 	This function gets the Inversed Matrix
## The usage of the assignment operator '<<-' will prevent the variables
## from being exposed to the outside world.
##
##************************* How to test the functions ***********************
## 1. Change the diretory in RStudio to the folder where cacheMatrix.R resides
## 2. Load the cacheMatrix.R:
## 	> source("/cachematrix.R")
## 3. Create a test matrix:
## 	> mTest <- matrix(runif(16,1,20),4,4)
## 4. Generate the makeCacheMatrix object:
##	> cTest <- makeCacheMatrix(mTest)
## 5. Solve for the inverse of the matrix:
##	> invTest <- cacheSolve(cTest)
## 6. If desired display inversed matrix
##	> invTest



makeCacheMatrix <- function(x = matrix()) {
	# This initializes the variable where the inversion result will be stored
	mInv <- NULL
      
	# The setter function
	set <- function(y) {
		x <<- y
		mInv <<- NULL
	} 
	
	# This gets the input matrix
	get <- function() x
      
	# This sets the inversed matrix
	setInv <- function(inv) mInv <<- inv

	# This gets the inversed matrix
      getInv <- function() mInv

	# The list allows the use of the fuctions from the makeCacheMatrix object
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)      
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	# Get the inverse of the matrix from object x otherwise return null
	m <- x$getInv()
      
	# Check if the inverse is returned
	if(!is.null(m)) {
	  message("getting cached data")
	  return(m)
      }
      
	# if the inverse is not returned
	# Get the matix, solve it, set it and return it
	data <- x$get()
      m <- solve(data)
      x$setInv(m)
      m
}
