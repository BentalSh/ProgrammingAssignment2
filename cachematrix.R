##The two functions bellow will be used to cache and solve matrices.
## makeCacheMatrix will create the cacheable matrix
## cacheSolve will either solve or use inverse with the matrix


## makeCacheMatrix returns a "cacheable" object. 
## The returned object should be used instead of the original matrix, with the cache solve function
makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL;
  
  set <-function(y) {
      ## Where y is the matrix we are using
      x<<-y;
      m<<-NULL;
  }
  get <- function() x
  setInverse <- function(inverse) m<<-inverse;
  getInverse <- function() m
  list (set=set, get=get, setInverse=setInverse, getInverse=getInverse);
}

## cacheSolve gets a "cacheable" matrix (should be create by makeCacheMatrix), and returns it's inverse
## If an inverse is not cached already, it will be calculated and cached.
## Otherwise the cached solution will be returned.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse();
  if (!is.null(m))
  {
    ##That is, we found a cached answer, we should return it
    message("getting cached data");
    m;
  }
  else
  {
    ##Inverse not found, we should compute it
    data <- x$get();
    m<-solve(data);
    ##Before returning the solution, we cache it for later use
    x$setInverse(m);
    m;
  }
}
