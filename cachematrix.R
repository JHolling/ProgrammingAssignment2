## caachematrix.R can be used to allow one inverted matrix "cached" for use
## for many calls and save time on not having to recalculate it.
##
## The makeCacheMatrix gets a matrix "m" and prepares it be to cached and utilizes
## List objects/functions and variables in parent enviroments to care this out.
##
## The cacheSolve function checks if the inverted matrix exists. If not, it will invert the "m"
## and pass it onward.
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
                invert_mat <<- NULL              #assinging NULL and Parent invert_mat variable 
        }
        get <- function() m
        setinverse <- function(solve) invert_mat <<- solve  
        getinverse <- function() invert_mat                 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks to see if inverted matrix exist. If not, returns an newly inverted matrix m. 

cacheSolve <- function(m, ...)  
{
        invert_mat <- m$getinverse()                   
        if(!is.null(invert_mat))                     #checks if inverted matrix exists
        {
                message("getting cached data")       
                return(invert_mat)  
        }
        data <- m$get()                              #if no, call fun get() to get local x "matrix
        invert_mat <- solve(data, ...)               #run solve fun to invert the "x" matrix                   
        m$setinverse(invert_mat)                     #updates newly inverted matrix
        invert_mat                                   
}
