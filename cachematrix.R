makeCacheMatrix <- function(x = matrix()) 
     {
     #Assign default of NULL to m and y
     m <- NULL
     y <- NULL
     
     #create functions to cache input and output
     set_input <- function(y) 
          {
          x <<- y
          m <<- NULL
          }
     get_input <- function() x
     set_output <- function(solve) m <<- solve
     get_output <- function() m 
     
     #create a list that can be referenced get functions for getting and setting
     #input and output caches
     list(set_input = set_input,
          get_input = get_input,
          set_output = set_output,
          get_output = get_output)
     }

cacheSolve <- function (x=matrix(), ...) 
     {
     #Assign inverse matrix from cache (if applicable)
     m <- x$get_output()
     #Check value of m to see a cache result was found
     #and if matrix has changed
     if(!is.null(m) && identical(x$get_output(),m)==TRUE)
          {
          #is not empty and no change so return cache with message
          message("getting cached data")
          return(m)
          }
     
     #No cache value was available or matrix has changed
     #so calculate inverse and cache input and output
     
     #Cache matrix being input to test
     y <- x$get_input()
     x$set_input(y)
     
     #compute the value of the inverse/output of the input matrix
     m <- solve(y, ...)
     #cache the output just calculated
     x$set_output(m)
     #return output just calculated
     m
     }  

