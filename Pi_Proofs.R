# Pi_Proofs.R -------------------------------------------------------
#' Purpose of script:
#'
## Date Created: 2025-04-03
##
## ## Email: PixelInsight@ProtonMail.com
##
## Notes: -------------------
#'
#' A walk through of three proofs of pi using base R
#' 
#'   
##
# Mathematical Explorations of π ------------------------------------------


n <- 25 # the number of iterations used in each proof


# Method 1 - Monte Carlo

monte_proof <- function(n) {
  set.seed(round(as.numeric(Sys.time())))
  pts <- matrix(runif(2*n), ncol=2)
  
  # Distance between coordinates from origin
  origin_dist <- pts[,1]^2 + pts[,2]^2 
  
      #' points in circle = sum(origin_dist <= 1)
      #' points in square = n
      #' π = 4 * points in circle / points in square
  
  return(4 * sum(origin_dist <= 1) / n)
}

# Result: 
pi_monte <- monte_proof(n)

cat(" *******", "\n", 
    "Monte Carlo π estimate:", round(pi_monte, 6), "\n",
    "Actual π:", round(pi, 6), "\n",
    "Percentage difference:", 
                    100*((pi_monte-pi)/ ((pi_monte+pi)*0.5)) |> # percentage calculation
                      abs() |> # absolute value
                      round(6), # rounding
    "%", "\n",
    "*******"
    )



#' Method 2 - Leibniz
#' π/4  =  1  - 1/3 + 1/5 - 1/7 + 1/9 ...


leibniz_proof <- function(n) {
  terms <- seq(0, n)
  result <- sum((-1)^terms / (2 * terms + 1))
  return(4 * result)
}


# Result: 
pi_leibniz <- leibniz_proof(n)

cat(" *******", "\n", 
    "Leibniz series π estimate:", round(pi_leibniz, 6), "\n",
    "Actual π:", round(pi, 6), "\n",
    "Percentage difference:", 
    100*((pi_leibniz-pi)/ ((pi_leibniz+pi)*0.5)) |> # percentage calculation
      abs() |> # absolute value
      round(6), # rounding
    "%", "\n",
    "*******"
)


# Method 3 - Archimedes'  
# here n represents the number of edges in a regular polygon
# assuming radius = 1

archimedes_proof <- function(n) {
  angle <- 2 * acos(-1) / n # angle in Radian  
  side_length <- sqrt(2 - 2 * cos(angle))  # law of cosines c² = a² + b² - 2ab * cos(C)
  
  perimeter <- n * side_length
  
  return(perimeter / 2) 
  #' dividing by two as radius is assumed at 1, 
  #' perimeter ~ circumference
  #' circumference = π x diameter = π x 2 x 1 
  #' π ~ perimeter / 2
  #' 
}

# Result: 
pi_archimedes <- archimedes_proof(n)

cat(" *******", "\n", 
    "Archimedes' π estimate:", round(pi_archimedes, 6), "\n",
    "Actual π:", round(pi, 6), "\n",
    "Percentage difference:", 
    100*((pi_archimedes-pi)/ ((pi_archimedes+pi)*0.5)) |> # percentage calculation
      abs() |> # absolute value
      round(6), # rounding
    "%", "\n",
    "*******"
)

