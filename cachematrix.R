## This program calculates the Inverse of the Matrix depending on whether 
## the data is cached in memory or not
## This saves the computation required to calculate the Inverse everytime
## that Inverse operation is called for a Matrix

## makeCacheMatrix() deals with the Basic Setter,Getter approach to the program
## If a Matrix is set, it sets in in a variable from which later it can 
## retrieve the values of the Matrix
## The Inverse values are set and get here to provide the Caching Mechanism

makeCacheMatrix <- function(x = matrix())
{
  m<-NULL
  set<-function(y)
  {
    	x<<-y
    	m<<-NULL
  }
  get<-function()x
  setinv<-function(cacheSolve)m<<-cacheSolve
  getinv<-function()m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve() gets the inverse of a matrix from the cache and if it is NULL
## gets the values for the Matrx via the Getter function, then calculates the 
## Inverse for the Matrix and sets it via the Setter Function
## later when we query the cacheSolve() function, it returns the Inverse 
## from the Cache, the sequence of operations can be seen in the Debugger Mode

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
m1<-x$getinv()
if(!is.null(m1))
{
 	 return(m1)
}
data<-x$get()
m1<-solve(data)
x$setinv(m1)
m1
}
