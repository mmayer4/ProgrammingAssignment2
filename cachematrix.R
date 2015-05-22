## The code below is a pair of functions written to accomplish 
## two tasks seqeuentially:
## 1 - Stores a matrix and contains a cache to store its inverse
        ## The next function is used to compute the inverse and store it,
        ## but only if the cash is empty

## 2 - Computes the inverse of a matrix, unless it already has been solved


## Here is the code for function 1 - Caching the inverse of a matrix

#makeCacheMatrix is in fact a list with 4 functions
makeCacheMatrix <- function(x = matrix()) {
       
        m <- NULL     #m will be our inverse and must be nullified
                      #when storing a new matrix
        
        
       
        set <- function(y){      #The function set(), allows us to change 
                x <- y           #the stored matrix;
                m <<- NULL       #The cached matrix must be reset if done so
                
               
        }
        
        
        get <- function() x      #get() returns the stored info and is called 
                                 #upon in the cacheSolve function below
        
        
        setinverse <- function(solve) m <<- solve
        #setinverse() allows us to cache the inverse manually, 
        #may not actually be the inverse if you do that!!
        
        
        getinverse <- function() m #getinverse returns the cached matrix
           
              
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
        
        #the list below is what creates the list of functions
        #found inmakeCacheMatrix
}




## 2 - Computing the inverse of a matrix


#For this function, we're computing the inverse of a matrix with solve()
#and storing it in the cash "m" of the previous list of functions
#Unless there is already a matrix in m,
#In that case, it is returned

cacheSolve <- function(x,...){
        m <- x$getinverse()     #Calls m from the makeCacheMatrix function
        
        if(!is.null(m)) {       #When there is a matrix in m...
                
                message("getting cached data")
                return(m)
        } else {                #If there is no inverse matrix it is "solved"
        
        data <- x$get()
        m<- solve(data, ...)
        message("returning solved matrix and caching it")
        x$setinverse(m)        #After being computed it is stored in m
        m                      #And printed to the console
        }
}

## Note, the use of the two functions are as follows:
## makeCacheMatrix(matrix) -> cach.matrix
## cacheSolve(cach.matrix)
## The inverse is computed, displayed and stored in the cache
## If it is already stored, it is printed but not computed


