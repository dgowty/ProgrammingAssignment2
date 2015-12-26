## Author: Doryan Gowty
## R Programming, Assignment 2
## Date: 27/12/2015
## 
## This program caches the inverse of a matrix
## for retreival without having to recompute
## results
## 

## create list of functions that takes and stores matrix, 
## computes inverse and stores results 
makeCacheMatrix <- function(X = matrix()) {
        
        M <- matrix(,nrow= nrow(X), ncol = ncol(X) ) #Empty matrix for holding inverse 
        
        setMat <- function(y) {
                X <<- y #assign input matrix
                M <<- matrix(,nrow= nrow(X), ncol = ncol(X) ) #clear cache from inverse 
        }#end function setMat
        getMat <- function() X 
        setInv <- function(invMat) M <<- invMat 
        getInv <- function() M
        
        list(
                setMat = setMat
                , getMat = getMat
                , setInv = setInv
                , getInv = getInv
        )
} #end function makeCacheMatrix

## Check for cached value of matrix inverse, return value
## if there is one, otherwise calculate new value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        
        if(!all(is.na(m))) {
                message("retrieving cached inverse")
                return(m)
        }
        
        message("no data cached")
        m <- solve(x$getMat())
        x$setInv(m) # cache results
        m 
} #end function cacheSolve
