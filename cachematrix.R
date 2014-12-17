## Motivation : Function create a list of 4 functions,
## which can be aplied to any type  of objects.
## When we need to caching some CPU consumed operation 
## I.e  A -> some CPU consumed function -> B, were A is sorce of data 
## and B is result of function aplied to A  
## In context of the class and task (Caching the Inverse of a Matrix),
## we will use matrix as source type (x),  and iverse matrix of x -> ix
##
## Input: n-by-n square matrix 
## 
## Output:  list of 4 functions:
## 1 get() - get the x matrix which must be inverted 
## 2 set(x) - set the x matrix -
## note this will remove any previous cacshe of ix
## 3 getInverse()  - get the inverse matrix of x, i.e ix or NULL,
## if it was not calculated yet 
## 4 setInverse(ix) set the ix 


makeCacheMatrix <- function(x = matrix()) {
        ##initalize with null ( empty ) inverse matrix 
        ix <- NULL 
        
        
        set <- function(y) {
                x <<- y       ##set new matrix 
                ix<<- NULL ## old inverse is no longer valid 
        }
        
        get <- function() { 
                x  ##just return current matrix
        }
        
        setInverse <- function(newIx = matrix()){
                ix <<- newIx     ##set new inverse matrix         
        }
        
        getInverse <- function() {
                ix ##just return current invert matrix of X
        }
        ## return the list of 4 functions     
        list(set = set, get = get,
             getInverse = getInverse,
             setInverse = setInverse)
        
}


## Function to solve a matrix. I.e to calculate a inverse matrix of given one.
## Note that the input matrix must be square n-by-n 
## for more info see : http://en.wikipedia.org/wiki/Invertible_matrix
##
## Input:  x, object which is matrix wrapper ( list of 4 functions ) 
## type list created by makeCacheMatrix
## 
## Output: the inverse matrix of x$get() 
## Function :  
## 1 Check if the inverse matrix of given input are allready calculated 
## if no 
##      a) calculate inverse by calling solve()
##      b) store reslt vial x$setInverse
## 2 return the inverse matrix ( cached or calculated )


cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of 'x'
        
        ix <- x$getInverse()  ##1-st get the inverse matrix from x
        
        ## if it is null, then  we need to calculate and cache it 
        if(is.null(ix)) {                         
                message("calclate inverse matrix and cache it.")
                data <- x$get() #get the src matrix first 
                ix <- solve(data, ...) ## solve it to obtain inverse of X ->ix 
                x$setInverse(ix)  #store inverce matrix for future needs          ]
        }
        ix
}