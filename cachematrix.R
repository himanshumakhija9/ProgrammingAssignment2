
makeCacheMatrix <- function(matrix1=matrix()){

  inv <- NULL                             
  set <- function(y) {                    
    matrix1 <<- y                             
    inv <<- NULL                        
  }
  get <- function() matrix1                     
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  
}


cacheSolve <- function(matrix1, ...){
      inv <- matrix1$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
      data <- matrix1$get()
      inv <- solve(data, ...)
      matrix1$setinverse(inv)
      inv
}
