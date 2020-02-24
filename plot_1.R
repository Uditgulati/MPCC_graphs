library(MPCC)
library(MPCCmkl)
library(rbenchmark)
cols <- c("test", "replications", "elapsed", "relative")
reps <- 20
set.seed(1)

sizes = c(100, 500, 1000, 2000, 4000, 8000)
cor_vals = c()
mkl_vals = c()
PCC_vals = c()
naive_vals = c()

ymax = 0

df<-data.frame()

for (i in 1:length(sizes)) {
  m <- sizes[i]
  n <- sizes[i]
  x <- matrix(rnorm(m*n), m, n)
  y <- matrix(rnorm(m*n), m, n)

  if(sizes[i] == 4000) {
    reps <- 4
  }
  if(sizes[i] == 8000) {
    reps <- 2
  }

  print(i)
  a = benchmark(cor(x, y), replications=reps, columns=cols)
  b = benchmark(MPCCmkl::PCC(x, y), replications=reps, columns=cols)
  c = benchmark(MPCC::PCC(x, y), replications=reps, columns=cols)
  d = benchmark(PCC.naive(x, y), replications=reps, columns=cols)

  print(i)
  cor_vals[i] = (a[1, 3] / reps)
  mkl_vals[i] = (b[1, 3] / reps)
  PCC_vals[i] = (c[1, 3] / reps)
  naive_vals[i] = (d[1, 3] / reps)

  entry<-data.frame(m, n, cor_vals[i], mkl_vals[i], PCC_vals[i], naive_vals[i])

  if(nrow(df) == 0) {
    df <- entry
  }
  else {
    df <- rbind(df, entry)
  }  

  ymax = max(ymax, cor_vals[i], mkl_vals[i], PCC_vals[i], naive_vals[i])
}

names(df) = c("m", "n", "cor", "mkl", "openblas", "naive")

args <- commandArgs()
filename <- paste(args[length(args)], ".csv", sep="")

write.csv(df, row.names = FALSE, filename)

png("plot_1.png", width=1024, height = 800)
 
# Make a basic graph
plot( cor_vals~sizes , type="b" , bty="l" , xlab="Matrix size" , ylab="Time(seconds)" , col=rgb(0.9,0.1,0.1,0.7) , lwd=3 , pch=17, ylim = c(0, ymax))
lines( mkl_vals~sizes , col=rgb(0.1,0.9,0.1,0.7) , lwd=3 , pch=18 , type="b" )
lines( PCC_vals~sizes , col=rgb(0.1,0.1,0.9,0.7) , lwd=3 , pch=19 , type="b" )
lines( naive_vals~sizes , col=rgb(0.3,0.3,0.3,0.7) , lwd=3 , pch=20 , type="b" )
 
# Add a legend
legend("bottomleft", 
  legend = c("cor", "MKL", "OpenBLAS", "naive"), 
  col = c(rgb(0.9,0.1,0.1,0.7), 
  rgb(0.1,0.9,0.1,0.7),
  rgb(0.1,0.1,0.9,0.7),
  rgb(0.3,0.3,0.3,0.7)), 
  pch = c(17, 18, 19, 20), 
  bty = "n", 
  pt.cex = 2, 
  cex = 1.2, 
  text.col = "black", 
  horiz = F , 
  inset = c(0.1, 0.1, 0.1, 0.1))

dev.off()
