## makeCacheMatrix function creates a matrix, which is a list of following functions
## set              : set the value of matrix
## get              : get the value of matrix
## setinversematrix : set the value of the inversematrix
## getinversematrix : get the value of the inversematrix

## This function takes a matrix as input argument and creates special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                 m<-NULL
                set<-function(y){
                  x<<-y
                  m<<-NULL
                }
                get<- function()x
                setinversematrix<-function(inverseMatrix){
                  m<<-inverseMatrix
                }
                getinversematrix<-function()m
                list(set=set,get=get,
                     setinversematrix=setinversematrix,
                     getinversematrix=getinversematrix)
}


## cacheSolve computes the inverse of the matrix. If inverse is already computed then cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m<-x$getinversematrix()
            if(!is.null(m)){
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m<-solve(data)
            x$setinversematrix(m)
            m
}
