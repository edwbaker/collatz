bigNumber <- 5000
oddDegChange <- 1.2
evenDegChange <- -0.54

oddRadChange <- oddDegChange * pi / 180
evenRadChange <- evenDegChange * pi / 180

l <- weight <- vector('integer', bigNumber)
collatz <- function(n) {
  weight[[n]] <<- weight[[n]] + 1
  if (n == 1) {
    return()
  }
  if (n %% 2 == 0) {
    l[[n]] <<- n/2
    collatz(n/2)
  } else {
    l[[n]] <<- 3*n+1
    if (3*n + 1 <= bigNumber) {
      collatz(3*n+1)
    }
  }
}

for (n in 1:bigNumber) {
  collatz(n)
}

startx <- starty <- endx <- endy <- vector('numeric', bigNumber)

drawCollatz <- function(n, x1, y1, rad1) {
  startx[[n]] <<- x1
  starty[[n]] <<- y1
  if (n %% 2 == 0) {
    rad2 <- rad1 + evenRadChange
  } else {
    rad2 <- rad1 + oddRadChange
  }
  endx[[n]] <<- x2 <- x1 + cos(rad2)
  endy[[n]] <<- y2 <- y1 + sin(rad2)
  
  nextValue <- which(l == n)
  if (length(nextValue) > 0) {
    for (i in 1:length(nextValue)) {
      drawCollatz(nextValue[[i]], x2, y2, rad2)
    }
  }
}

drawCollatz(2, 0, 0, 0)

plot.new()
plot.window(xlim=c(min(endx),max(endx)), ylim=c(min(endy),max(endy)))

for (n in 1:bigNumber) {
  segments(startx[[n]],starty[[n]],endx[[n]],endy[[n]],lwd=0.5, col="purple")
  #points(endx[[n]],endy[[n]])
}
