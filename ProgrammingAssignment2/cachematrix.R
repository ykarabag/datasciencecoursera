## A sample exercise showing how expensive computing operations can be reduced by using caching of variables

## Create functions to set & get the values of matrix as well as inverse of it

makeCacheMatrix <- function(x = matrix()) {
        
        m<-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get<-function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
}


## This function assumes matrix is always invertible and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)){
                
                message("getting cached inverse data")
                return (m)
                
        }
        
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}


## To run the samples use the following code:
## a <- rbind(c(1,2), c(3,4))
## b <- makeCacheMatrix(a)
## print(cacheSolve(b))
## print again
## print(cacheSolve(b))
## observe the caching message