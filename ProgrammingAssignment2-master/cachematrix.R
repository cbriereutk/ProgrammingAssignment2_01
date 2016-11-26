## The function makeCacheMatrix will take in a square matrix of any size and create a matrix object
## that is able to cache its own matrix.
## The function cacheSolve then takes the matrix object from makeCacheMatrix and finds its inverse.
## If the inverse has already been calculated then it will get the inverse from the cache

## Takes in a matrix 
makeCacheMatrix <- function(x = matrix()) {
        ## Initializes the inverse matrix by allocating memory for it and setting the value to NULL.
        I<-NULL
        ## The variables "x" and "I" exist in the parent environment, so when set is called upon from cacheSolve
        ## this function replaces previously stored values.
        set<-function(y){
                x<<-y
                I<<-NULL
        }
        
        ## This function when called gets the value of "x" from the parent environment
        get<-function() x
        
        ## This function when called sets the value of the inverse "I".
        ## "I" is defined in the parent environment so after we set the value of "inv" the code
        ## accesses that value and stores it as "I" in the parent environment
        setinv<-function(inv) I<<-inv
        
        ## Similar to the getter for "x" this function finds the correct value of "I"
        getinv<-function() I
        
        ## This assigns each of the functions as a list element
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function takes in arguments of type makeCacheMatrix
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        I<-x$getinv()
        
        ## Checks if inverse has already been calculated and stored
        if(!is.null(I)){
                message("Getting cache data")
                return(I)
        }
        
        ## If "I" is NULL then "data" grabs the value of the matrix "x" from
        ## the parent environment
        data<-x$get()
        
        ## "I" is then solved for using the inverse function solve(x=matrix(),...)
        I<-solve(data,...)
        
        ## Sets value of "I" to its symbol in parent environment
        x$setinv(I)
        
        ## Returns inverse matrix
        I
}

## Verify the inverse matrix was found
verifyCacheSolve <-function(A,B){
        ## "A" is the original matrix and "B" is the calculated inverse.
        ## The "%*%" is matrix multiplication
        Soln<-A%*%B
        
        ## Solution without rounding
        print(Soln)
        ## The round() function takes care of the machine epsilon difference
        print(round(Soln))
}
