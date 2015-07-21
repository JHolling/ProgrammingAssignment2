## cachematrix.R can be used to allow one inverted matrix "cached" for use
## on many calls and save time on not having to recalculate it.
##
## function usage:
## m<-matrix(c(1,2,2,1),nrow=2,ncol=2)
## matrix<-makeCacheMatrix(m)
## cacheSolve(matrix) 
##
## The "makeCacheMatrix" function, gets a matrix "x", sets it to a variable, and defines
## a special List of objects/functions to read & write the current matrix as well 
## as read & write the existing inverted matrix (if any).
 
makeCacheMatrix <- function(x = matrix())
{
        invert_mat <- NULL
        set <- function(y) 
        {                
                x <<- y                          # assigning local variable to 
                invert_mat <<- NULL              # parent variables
        }
        get <- function() x
        setinverse <- function(solve) invert_mat <<- solve  
        getinverse <- function() invert_mat                 
        list(set = set, get = get,              
             setinverse = setinverse,
             getinverse = getinverse)
}

## The "cacheSolve" function takes the matrix from makeCacheMatrix, and uses several 
## functions from the above Special List of Objects/functions to determing if the inverted
## matrix already exists. If not, it will invert the "x" with an R function 
## called "Solve" and set it as the new cached inverted matrix.

cacheSolve <- function(x, ...)  
{
        invert_mat <- x$getinverse()                   
        if(!is.null(invert_mat))                     #checks if inverted matrix exists
        {
                message("getting cached data")       
                return(invert_mat)  
        }
        data <- x$get()                              #if no, call get() to get local x "matrix
        invert_mat <- solve(data, ...)               #and run solve to invert it.                   
        x$setinverse(invert_mat)                     
        invert_mat                                   
}
