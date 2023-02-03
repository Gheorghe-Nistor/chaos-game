library(shiny)

chaosGame <- function(n, r, restriction, nr_points, dot_color, dot_size, delay) {
  cat("Start \n")
  initial_points_x <- numeric()
  initial_points_y <- numeric()
  
  # punctele din care este formată figura geometrică inițială
  for (i in 1:n) {
    angle =  (i - 1) * 2 * pi / n;
    initial_points_x[i] <- 5 * sin(angle)
    initial_points_y[i] <- 5 * cos(angle)
  }
  
  # în points_x și points_y vom memora toate punctele din interiorul figurii geometrice
  points_x <- numeric()
  points_y <- numeric()
  
  # un punct fixat din interiorul figurii geometrice
  current_x <- initial_points_x[1]
  current_y <- initial_points_y[1] - 1
  
  last_p = 0
  
  for(i in 1:nr_points){
    points_x <- append(points_x, current_x)
    points_y <- append(points_y, current_y)
    
    # aleg un punct random din fig. geometrică inițială
    p <- sample(1:n, 1)
    
    if (restriction == TRUE) {
      while (p == last_p) {
        p <- sample(1:n, 1)
      }
    }
    
    last_p = p
    
    # calculăm un punct nou
    current_x <- initial_points_x[p] + r * (current_x - initial_points_x[p])
    current_y <- initial_points_y[p] + r * (current_y - initial_points_y[p])
    
    if(delay > 0){
      x = append(points_x, initial_points_x)
      y = append(points_y, initial_points_y)
      plot(x, y,
           xlab = "",
           ylab = "",
           type = "p", # puncte, nu linii
           col = dot_color,
           pch = 16, # tipul simbolului
           cex = dot_size)
      Sys.sleep(delay);
    }
  }
  
  #debug
  # cat("points_x: ", points_x, "\n")
  # cat("points_y: ", points_y, "\n")
  
  if(delay == 0){
    x = append(points_x, initial_points_x)
    y = append(points_y, initial_points_y)
    
    plot(x, y,
         xlab = "",
         ylab = "",
         type = "p", # puncte, nu linii
         col = dot_color,
         pch = 16, # tipul simbolului
         cex = dot_size)
  }
  cat("Stop \n")
}

# cu cât numărul de puncte este mai mare, iar mărimea punctului este mai mică, cu atât desenul va fi mai clar
chaosGame(n=3, r=1/2, restriction=FALSE, nr_points=1000, dot_color="darkgreen", dot_size=1.5, delay=1)
