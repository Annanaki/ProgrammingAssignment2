## A pair of functions that cache the inverse-value of a matrix to avoid computing it repeatedly:
## makeCacheMatrix - this function creates a special "matrix" object that can cache its inverse
## cachesolve - this function computes the inverse-value of the matrix returned by the makeCacheMatrix function


##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {				#set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <-function() x				#get the value of the matrix
        setmatrix <- function(solve) m <<- solve	#set the inverse-value of the matrix
        getmatrix <- function() m			#get the inverse-value of the matrix
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Computes the inverse of the matrix returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {                
        ## Return a matrix that is the inverse of 'x'

        m <- x$getmatrix()			#first checks to see if the inverse-value of the matrix has already been calculated
        if(!is.null(m)) {			#gets the inverse-value from the cache and skips the computation
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()			
        m <- solve(matrix, ...)			#calculates the inverse-value of the matrix
        x$setmatrix(m)				#sets the inverse-value of the matrix in the cache via the setmatrix function
        m                
}

