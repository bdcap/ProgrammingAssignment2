## These functions make use of cached results to avoid having to compute 
## computationally intensive processes like matrix inversion repeatedly.
## Instead, these two functions below: a. generate a version of an inverted matrix
## in the cache and then b. recall that matrix from the cache (instead of having
## to calculate it again).

## The function makeCacheMatrix has essentially four steps, namely that it:
## 1. sets the values of the matrix, 
## 2. gets the values of the matrix, 
## 3. sets the values of the inverse of the matrix, 
## 4. gets the values of the inverse of the matrix

makeCacheMatrix  <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## The function CacheSolve retrieves the inverse of the matrix from the cache if that data is available 
## (i.e., if the inverse has already been calculated and the matrix hasn't changed). Otherwise, the function
## retrieves the inverse from the cache, including a message indicating that it is "getting cached data". 

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}

## Testing

#Defining the matrix T
T <- matrix(c(1:4),2,2)

T_i <- makeCacheMatrix(T)
cacheSolve(T_i) 

# Checking to see that, once computed, the cacheSolve function will retrieve the stored information from the cache
cacheSolve(T_i)

# It does, per the message "getting cached data"
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5