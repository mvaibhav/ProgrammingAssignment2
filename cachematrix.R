
## Programming Assignment
## ----------------------
## Assignment: Caching the Inverse of a Matrix

## This source includes 2 functions
## 1 - makeCacheMatrix - constructor class
## 2 - cacheSolve - calculates inverse of matrix (See function description for further info)

## Please note that for the below to work the Matrix must be invertible else solve will throw an error
## especially if your input is a singular matrix


## The makeCacheMatrix function is used to create 
## sets and gets the Matrix
## sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        invm <- NULL
        ## the change Flag is used to denote if a matrix has been changed or not. 
        ## Before retrieving cache we check on this
        changeFlag <- NULL
        
        ## Set and get functions
        set <- function(y) {
                x <<- y
                invm <<- NULL
                changeFlag <<- "Y" ## Whenever new matrix is set or changed set this flag
        }
        get <- function() x
        
        ## Below set and get functions to retrieve inverse of matrix
        setInverse <- function(solve) invm <<- solve
        getInverse <- function() invm
        
        ## Below set and get functions to retrieve changed Flag status
        getChangeFlag <- function() changeFlag
        setChangeFlag <- function(t) changeFlag <<- t
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse,
             getChangeFlag = getChangeFlag,
             setChangeFlag = setChangeFlag)
}


## Cachesolve function uses the solve function to calculate the inverse of the Matrix
## In the process, it validates if the original matrix has been changed
## If not then it gets the inverse matrix from the cache and skips the computation.
## Else, it calculates, in case where matrix was changed, recalculates the inverse
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getInverse()
        
        ## Check if object present or if changeFlag was set
        if(!is.null(invm) && x$getChangeFlag()=="N") {
                message("getting cached inverse matrix")
                return(invm)
        }
        
        message("inverse not cached... creating cache")
        
        data <- x$get()
        data
        invm <- solve(data, ...) ## get inverse
        x$setInverse(invm)
        
        ## Since Inverse has been calculated set changeFlag back to N
        x$setChangeFlag("N")
        invm ##Return inverse
        
}

