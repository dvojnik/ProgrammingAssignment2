## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix is basically vector of 4 functions. To run it needs the matrix as arg.
##	set - which can reset the matrix and set inverse to null
##	get - which prints out matrix
##	setinverse - sets the "global" inverse matrix
##	getinvers - prints inverse matrix

##cacheSolve is a function which 
##in first step it checks the value of $getinverse 
##and if it is NULL then the calculation is made - solve
##and written back via $setinverse
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data.")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}