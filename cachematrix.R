makeCacheMatrix <- function(m = matrix()) {
    m_inv <- NULL
    set <- function(m_in) {
        m <<- m_in
        m_inv <<- NULL
    }
    get <- function() m
    set_inv <- function(m_inv_in) m_inv <<- m_inv_in
    get_inv <- function() m_inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(m_in, ...) {
    m_inv <- m_in$get_inv()
    
    if(is.null(m_inv)) {
        m = m_in$get()
        m_inv = solve(m)
        m_in$set_inv(m_inv)
        m_inv
        
    } else {    
        m_inv
    }
}
