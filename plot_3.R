library(MPCC)
library(rbenchmark)
cols <- c("test", "replications", "elapsed", "relative")
reps <- 20
set.seed(1)

sizes = c(100, 500, 1000, 2000, 4000, 8000)
one_vals = c()
two_vals = c()

ymax = 0

df<-data.frame()

for (i in 1:length(sizes)) {
  m <- sizes[i]
  n <- sizes[i]
  x <- matrix(rnorm(m*n), m, n)

  if(sizes[i] == 4000) {
    reps <- 4
  }
  if(sizes[i] == 8000) {
    reps <- 2
  }

  print(i)
  a = benchmark(PCC(x), replications=reps, columns=cols)
  b = benchmark(PCC(x, x), replications=reps, columns=cols)

  print(i)
  one_vals[i] = (a[1, 3] / reps)
  two_vals[i] = (b[1, 3] / reps)

  entry<-data.frame(m, n, one_vals[i], two_vals[i])

  if(nrow(df) == 0) {
    df <- entry
  }
  else {
    df <- rbind(df, entry)
  } 

  ymax = max(ymax, one_vals[i], two_vals[i])
}

names(df) = c("m", "n", "one_input", "two_inputs")

args <- commandArgs()
filename <- paste(args[length(args)], ".csv", sep="")

write.csv(df, row.names = FALSE, filename)

png("plot_3.png", width=1024, height = 800)
 
# Make a basic graph
plot(one_vals~sizes, type="b", bty="l", xlab="Matrix size", ylab="Time(seconds)", col=rgb(0.9,0.1,0.1,0.7), lwd=3, pch=17, ylim = c(0, ymax))
lines(two_vals~sizes, col=rgb(0.1,0.9,0.1,0.7), lwd=3, pch=18, type="b")
 
# Add a legend
legend("bottomleft", 
  legend = c("One input", "Two inputs"), 
  col = c(rgb(0.9,0.1,0.1,0.7), 
  rgb(0.1,0.9,0.1,0.7)), 
  pch = c(17, 18), 
  bty = "n", 
  pt.cex = 2, 
  cex = 1.2, 
  text.col = "black", 
  horiz = F , 
  inset = c(0.1, 0.1))

dev.off()
