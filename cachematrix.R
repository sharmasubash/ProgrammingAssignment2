## The aim is to use caching mechanism of R, if the same code is running again and again 
## R should pick the output from the first execution, instead of running code
## it will help to reduce the processing time as well as the memory resources

#first i will build the function to create a cache matrix

makeCacheMatrix <- function(x = matrix()) {
var <- NULL
set <- function(z) {
  x <<- z
  var <<- NULL
}
get <- function() x
setinverse <- function(inverse) var <<- inverse
getinverse <- function() var
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Once we have our cache matrix, we can inverse it using the below function. If the matrix
## is inversed it will return it from the cache otherwise it will run code and return the output

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 var <- x$getinverse()
 if(!is.null(var))
 {
   message("cached data")
   return(var)
 }
 data <- x$get()
 var <- solve(data)
 x$setinverse(var)
 var
}

##sample output

## create a vector
##x = rbind(c(1, -4), c(-4, 1))

## make a matrix

##matrix = makeCacheMatrix(x)
##matrix$get()

##      [,1] [,2]
##[1,]    1   -4
##[2,]   -4    1

##get the inverse

##cacheSolve(matrix)

##            [,1]        [,2]
##[1,] -0.06666667 -0.26666667
##[2,] -0.26666667 -0.06666667

##Rerun the code again

##cacheSolve(matrix)

##cached data

##            [,1]        [,2]
##[1,] -0.06666667 -0.26666667
##[2,] -0.26666667 -0.06666667





