#This functions caches the matrix. This function is similar to makeVector function of the example
makeCacheMatrix <- function(x = matrix()) #default matrix is set as null
{
  inverse_matrix <- NULL
  set <- function(y) 
  {
    x <<- y #storing the matrix in cache memory
    inverse_matrix <<- NULL
  }
  get <- function() 
  {
    x #returning the matrix
  }
  set_inverse <- function(z) 
  {
    inverse_matrix <<- z # setting the inverse of the matrix in cache memory 
  }
  
  get_inverse <- function()
  {
    inverse_matrix # returning the inverse of matrix
  }
  list(set = set, get = get,set_inverse = set_inverse,get_inverse = get_inverse) # returning the list so as to retrive specific functionalities
}

#This function create inverse of the matrix. This function is similar to cacheMean
cacheSolve <- function(x, ...) #a matrix is required to inverse a function. "..." are provided since solve has many arguments apart from passing single matrix
{
  inverse_matrix <- x$get_inverse() # if the inverse is already done we will assign the value directly
  if(!is.null(inverse_matrix)) #if the matrix is already reversed we will return cached data
  {
    print("Returning cached data")
    return(inverse_matrix)
  }
  matrix_get <- x$get()
  inverse_matrix <- solve(matrix_get, ...)#inverse of matrix
  x$set_inverse(inverse_matrix) # setting inverse in cache memory
  inverse_matrix
}

#Output
#> x <- matrix(1:4,nrow = 2, ncol = 2)
#> x
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> m <- makeCacheMatrix(x)
#> cacheSolve(m)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
 