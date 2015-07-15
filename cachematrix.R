## the makeCacheMatrix gets a "x" matrix and calls a function from cacheSolve
## to return the inverted matrix of "x" if it exists. If inverted matrix doesn't exist,
## create one from x and set it the to the cache. 

makeCacheMatrix <- function(x, ...) 
{
        invert_mat <- NULL
        set <- function(y) 
        {                
                x <<- y                          #assigning local variable to parent variables
                invert_mat <<- NULL              #assinging NULL and Parent invert_mat variable 
        }
        get <- function() x
        setinverse <- function(solve) invert_mat <<- solve  
        getinverse <- function() invert_mat                 
        list(set = set, get = get,
             setinverse = setinvers,
             getinverse = getinverse)
}


## cacheSolve contains a set of List object/functions & variables in the parent enviroments to set/get 
## and set/get inverse. These functions are called from makeCacheMatrix 

cacheSolve <- function(x = matrix()) 
{
        invert_mat <- x$getinverse()                   
        if(!is.null(invert_mat))                     #checks if inverted matrix exists
        {
                message("getting cached data")       #if yes, "getting cached data" and return
                return(invert_mat)  
        }
        data <- x$get()                              #if no, call fun get() to get local x "matrix
        invert_mat <- solve(data, ...)               #run solve fun to invert the "x" matrix                   
        x$setinverse(invert_mat)                     #updates newly inverted matrix
        invert_mat                                   #shows what's in "invert_mat" and ends.
}
