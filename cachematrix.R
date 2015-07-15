## calling makeCacheMatrix gets a "x" matrix and calls a function from cacheSolve
## to return the inverted matrix of "x" if it exists. If inverted matrix doesn't exist,
## create one from x and set it the to the cache. 

makeCacheMatrix <- function(x = matrix()) {
        invert_mat <- x$getinverse()                   #gets any cached inverted matrix and sets to local "im" 
        if(!is.null(invert_mat))                       #checks if "im" exists or not
        {
                message("getting cached data") 
                return(invert_mat)                     #leave fun cacheSolve with existing "im" inverted matrix
        }
        data <- x$get()            #if no "im" cached existing, call fun get() to get non-inverted x "matrix"
        invert_mat <- solve(data, ...)     #run solve fun to Invert the matrix and place in "im" variable
        x$setinverse(invert_mat)           #call setinverse() to set newly inverted matrix to parent "im"
        invert_mat                         #send "im" to screen and end.

}


## cacheSolve contains sets List & parent enviroment variables and several functions to set/get 
#and set/get inverse. These functions are called from makeCacheMatrix 


cacheSolve <- function(x, ...) {
        invert_mat <- NULL
        set <- function(y) {                
                x <<- y                            #assigning variable to parent
                invert_mat <<- NULL                        #assinging variable to NULL and Parent
        }
        get <- function() x
        setinverse <- function(solve) invert_mat <<- solve  #calling to invert matrix 
        getinverse <- function() invert_mat                 # invert matrix
        list(set = set, get = get,
             setinverse = setinvers,
             getinverse = getinverse)
}