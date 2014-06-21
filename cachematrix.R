## Cloned file from the github repository. Locally edited the file to create the functions for the Programming Assignments 2
## This file contains 3 functions namely:
## 1. makeCacheMatrix  - Function to compute inverse matrix. Uses the <<- operator to assign values to variables in a different environment
## 2. inverse_Matrix   - Function returns inverse of a square matrix; for example 2X2 or 3X3
## 3. cacheSolve       - Function accepts a matrix as argument and returns a inverse of the matrix after calculating. Returns the inverse from Cache if already calculated and available

## --- Function to compute matrix and assign to cache variable
makeCacheMatrix <- function(matrix = numeric()){
        # --- Function accepts a matrix (assumes a square matrix) to compute 
        ## ---inverse. Uses the <<- operator to assign values to variables in a  
        # --- different environment.
          
        # --- set the inverse variable
        inverse <- NULL

        # --- set a new matrix to find a inverse; inverse is null for new matrix   
        setnewmatrix <- function(newmatrix) {
                matrix <<- newmatrix
                inverse <<- NULL
        }
        
        # --_ function retrieves the matrix
        getmatrix <- function() {
                matrix
        } 
        
        # --- Set a new inverse matrix 
        setnewinverse <- function(newinverse){
                inverse <<- newinverse
        }        
        
        # --- Retrieves the inverse matrix, which was calculated
        getinverse <- function() inverse
        
        # --- returns a list of functions
        list(setnewmatrix = setnewmatrix, getmatrix = getmatrix,
             setnewinverse = setnewinverse,
             getinverse = getinverse)
}

## --- Function returns inverse of a square matrix; for example 2X2 or 3X3
inverse_Matrix <- function(x,...){
        # --- returns the inverse of a square matrix
        solve(x)  
}


## --- Function returns an inverse of a matrix (By Computation or from Cache)
cacheSolve <- function(matrix,...){
        # --- Function accepts a matrix as argument and returns a inverse of the
        # --- matrix after calculating. Returns the inverse from Cache if already
        # --- calculcated and available
        
        # --- get the inverse of the matrix from the cache which holds the already calculated inverse
        inverse <- matrix$getinverse()
        
        # --- Check if the inverse is !NULL then return from Cached data
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # --- Else Calculate inverse using the inverse_Matrix function
        data <- matrix$getmatrix()
        
        inverse <- inverse_Matrix(data, ...)  # --- compute inverse
        
        matrix$setnewinverse(inverse)
        
        # --- Return a matrix that is the inverse of 'matrix' passed in the argument
        inverse
}