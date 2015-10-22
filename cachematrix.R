# This function stores a matrix and the inverse of the
# matrix.
# The matrix and the inverse of the matrix are stored in the cache.
# This makes sure that the matrix inversion is calculated only
# once when the matrix does not change.
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


# This function calculates the inverse of the matrix.
# The inverted matrix is stored in the cache.
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
