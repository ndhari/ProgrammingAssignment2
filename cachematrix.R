## The following functions (makeCacheMatrix and cacheSolve) cache the inverse of a matrix, in order to speed up computation.

## This function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.
#Args: x, an inversible matrix for which the inverse will be cached
#Returns: A list of functions for caching the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 
        s<-NULL
        
        # Sets value of the matrix
        set<-function(y){  
                x<<-y
                s<<-NULL
        }
        
        # Gets the value of the matrix
        get<-function() x  
        
        # Sets the inverse of the matrix
        setInverse<-function(solve) s<<-solve 
        
        # gets the inverse of the matrix
        getInverse<-function() s  
        
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
        
        # retrieves inverse as set by makeCacheMatrix function      
        s<-x$getInverse()  
        
        # If inverse was cached, returns the following message and the inverse      
        if(!is.null(s)){ 
                message("getting cached data")
                return(s)
        }
        
        # If no inverse was cached, computes inverse and caches it
        data<-x$get() 
        s<-solve(data)
        x$setInverse(s)
        
        # Return a matrix that is the inverse of 'x'
        s
        
}

