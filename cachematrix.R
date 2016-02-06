## These functions allow the inverse of a matrix to be cached to avoid repeating the calculation.
## 



## makeCacheMatrix takes a matrix input and create a 4 element list. The first element, set, is a function which sets the 
##stored value of the original matrix ( the matrix to be inverted), and sets the value of inv ( the inverse) to null fro new
##matrices. The second element, get, is a function that returns 
## the orinial matrix. The third element, setinv is a function that stores the inverse after it is calculated and a null
## value before. The fourth element, getinv, is a function that return the stored value for the inverse, either the actual
##inverse or the null value place holder.  The fourth element, getinv


makeCacheMatrix <- function(x=matrix()) {
  inv <-NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## CacheSolve usese the list created by makeCache matrix. It will return the inverse if it has already been calculated.
## If the inverse has not been calculated, it finds the inverse and uses the setinv function inside of makeCacheMatrix
## to store the inverse.

cacheSolve <- function(x,...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
