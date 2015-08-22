# ***** OVERALL DESCRIPTION****** :

# Matrix inversion is usually a costly computation  specially for a large matrix. There may be some benefit to caching 
# the inverse of a matrix rather than compute it repeatedly.
# The following two R functions (makeCacheMatrix  and cacheSolve) can be used to create a matrix that can cache its inverse 
# by calculting it only one time  and return it many times as needed.



# ***** makeCacheMatrix FUNCTION DESCRIPTION *****:

# The makeCacheMatrix  function is designed to help the caching of matrix inverse by creating a special "matrix" 
# object that can cache its inverse. The input and the return value of the function are  described  as follows:

# Usage:
#        L=makeCacheMatrix (m)

#            m: 	The input of the function is a variable of type matrix. 
#            L:	a special "matrix" object which is a list of four functions :
#                   1- set: set the input matrix.
#                   2- get :get the existing matrix. The initial value is Null.
#                   3- setInverse :Set the inverse matrix.
#                   4- getInverse :get the inverse matrix .

# Example and test:
# > m= matrix(c(1,2,3,4),nrow=2,ncol=2)
# > L=makeCacheMatrix (m)
# > L$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > L$getInverse()
# NULL
# > L$setInverse(solve(m))
# > L$getInverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > m2= matrix(c(5,6,7,8),nrow=2,ncol=2)
# > L$set(m2)
# > L$get()
# [,1] [,2]
# [1,]    5    7
# [2,]    6    8
# > L$getInverse()
# NULL

makeCacheMatrix <- function(x = matrix()) {

              inv <- NULL                                   #Initialize the inv to null 
              
              set <- function(y) {                          #Create the set function
                     x <<- y
                    inv <<- NULL
              }
              
              get <- function() x                           #Create the get function
              
              setInverse <- function(value) inv <<- value   #Create the setInverse function
              
              getInverse <- function() inv                  #Create the getInverse function 
              
                                                            #Return a list includes all of the above functions
             
               list(set = set, get = get,
                   setInverse = setInverse,
                   getInverse = getInverse)
            }
            


# ******cacheSolve FUNCTION DESCRIPTION *****:

# This function computes the inverse of the special "matrix" object  that can be created by the makeCacheMatrix function.
# If the inverse has already been calculated and the matrix has not been changed, then the cacheSolve 
# function  retrieves the inverse from the cache without recalculating  it. If the inverse has NOT
# been calculated or the matrix has been changed then the cachSolve will recalculate the inverse. 

# Usage:
#       inv=cacheSolvbe(L)
#             L:  is a special "matrix" object can be created using  makeCacheMatrix
#           inv: the inverse of the matrix in the special "matrix" object.

# Example and test:

# > m= matrix(c(1,2,3,4),nrow=2,ncol=2)
# > L=makeCacheMatrix (m)
# > L$getInverse()
# NULL
# > inv=cacheSolve(L)
# > inv
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > #Now let us try to call cacheSolve without changing L
# > inv2=cacheSolve(L)
# getting cached matrix inverse
# > inv2
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > Notice that we get the message "getting cached matrix inverse" to indicate that the cacheSolve does not recalculate the inverse
  


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()                         # Get the  inverse and store it in inv so that we can check its value 
  if(!is.null(inv)) {                           # Check if the inv is not null
    message("getting cached matrix inverse")    # Display a message
    return(inv)                                 # return the inv
  }
  data <- x$get()                               # Get the matrix and store it in data
  inv <- solve(data, ...)                       # Inverse the matrix data using the solve function
  x$setInverse(inv)                             # Set the inverse of the matrix 
  inv                                           # Return the  inv
}



