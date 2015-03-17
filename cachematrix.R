## Put comments here that give an overall description of what your
## functions do

## In order to store (cache) an inverse of a matrix and not have
## to recaculate it if it already exists, makeCacheMatrix.setsolve uses
## <<- operator to stored the value in the parent environment.
## If the value exists  - !is.null(s) - the stored value is returned

## Write a short comment describing this function
## makeCacheMatrix creates an object with associated getters and setters 
## which allow propertties of that object to be stored in the parent environment

makeCacheMatrix <- function(cacheMatrix = matrix()) {
      
      stored_result <- NULL ## no inverse calculation when cacheMatrix is created
      
      set <- function(y) { ## reset cacheMatrix to first state
            data_input <<- y
            stored_result <<- NULL
      }
      
      get <- function() cacheMatrix ## returns cacheMatrix 
      
      setsolve <- function(solved_matrix) stored_result <<- solved_matrix
      ## sets a stored result in the parent environment
      
      getsolve <- function() stored_result ## returns parent env stored_result
      
      ## prints list of cachmatrix functions
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## Write a short comment describing this function
## cacheSolve calls getsolve on the data passed to it. 
## If the result pre-exists it is returned
## if not - i.e is.null(stored_result), data is inverted usng "solve" and 
## passed to setsolve for caching

cacheSolve <- function(data_input, ...) {  
      
      stored_result <- data_input$getsolve() ## retrieve stored_result
      
      if(!is.null(stored_result)) { ## but only if it exists
            
            message("getting cached data")
            return(stored_result) ## return cached result
      }
      
      else ## The stored result hasn't beed created yet
            
      {
            data <- data_input$get() ## get the cachMatrix data
            
            ## assign the inverse of the cachMatrix data to var solved_matrix
            solved_matrix <- solve(data, ...) 
            
            ## send solved_matrix to setsolve for caching in parent env
            data_input$setsolve(solved_matrix)
       
            solved_matrix ##return result (not from cache)
            
      }
      
      
}
