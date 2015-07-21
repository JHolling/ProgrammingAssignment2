## cachematrix.R can be used to allow one inverted matrix "cached" for use
## on many calls and save time on not having to recalculate it.
##
## The makeCacheMatrix, gets a matrix "m" and defines a special List of objects/functions to 
## read & write the current matrix as well as read & write the existing inverted matrix (if any).
##
## The cacheSolve function takes the matrix from makeCacheMatrix, and uses several 
## functions from the above Special List of Objects/functions to determing if the inverted
## matrix already exists. If not, it will invert the "m" with an R function 
## called "Solve" and set it as the new cached inverted matrix.
##
## function usasge:
## m<-matrix(c(1,2,2,1),nrow=2,ncol=2)
## matrix<-makeCacheMatrix(m)
## cacheSolve(matrix) 

makeCacheMatrix <- function(m = matrix())
        {
        invert_mat <- NULL
        set <- function(y) 
        {                
                m <<- y                          #assigning local variable to parent variables
                invert_mat <<- NULL               
        }
        get <- function() m
        setinverse <- function(solve) invert_mat <<- solve  
        getinverse <- function() invert_mat                 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks to see if inverted matrix exist. If not, sets & returns an newly inverted matrix m. 

cacheSolve <- function(m, ...)  
{
        invert_mat <- m$getinverse()                   
        if(!is.null(invert_mat))                     #checks if inverted matrix exists
        {
                message("getting cached data")       
                return(invert_mat)  
        }
        data <- m$get()                              #if no, call get() to get local m "matrix
        invert_mat <- solve(data, ...)               #and run solve to invert it.                   
        m$setinverse(invert_mat)                     
        invert_mat                                   
}
