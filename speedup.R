# plot 1

filename <- "data_1.csv"

df <- read.csv(filename)

print(df)

sizes = c(100, 500, 1000, 2000, 4000, 8000)
cor_vals = c()
mkl_vals = c()
PCC_vals = c()
naive_vals = c()

ymax = 0
ymin = 100

for (i in 1:length(sizes)) {
  m <- sizes[i]
  n <- sizes[i]

  cor_vals[i] = (df[i, 3]-df[i, 3])*100.00 / df[i, 3]
  mkl_vals[i] = (df[i, 3]-df[i, 4])*100.00 / df[i, 3]
  PCC_vals[i] = (df[i, 3]-df[i, 5])*100.00 / df[i, 3]
  naive_vals[i] = (df[i, 3]-df[i, 6])*100.00 / df[i, 3]

  if(i >= 3) {
    ymax = max(ymax, cor_vals[i], mkl_vals[i], PCC_vals[i], naive_vals[i])
    ymin = min(ymin, cor_vals[i], mkl_vals[i], PCC_vals[i], naive_vals[i])
  }
}

png("speedup_plot_1.png", width=1024, height = 800)

print(mkl_vals)
print(PCC_vals)
print(naive_vals) 

# Make a basic graph
plot(mkl_vals[3:6]~sizes[3:6], type="b", bty="l", xlab="Matrix size", ylab="% Speedup", col=rgb(0.9,0.1,0.1,0.7), lwd=3, pch=17, ylim = c(ymin, ymax))
lines(PCC_vals[3:6]~sizes[3:6], col=rgb(0.1,0.1,0.9,0.7), lwd=3, pch=19, type="b")
lines(naive_vals[3:6]~sizes[3:6], col=rgb(0.3,0.3,0.3,0.7), lwd=3, pch=20, type="b")
 
# Add a legend
legend("bottomleft", 
  legend = c("MKL", "OpenBLAS", "naive"), 
  col = c(rgb(0.9,0.1,0.1,0.7),
  rgb(0.1,0.1,0.9,0.7),
  rgb(0.3,0.3,0.3,0.7)), 
  pch = c(17, 19, 20), 
  bty = "n", 
  pt.cex = 2, 
  cex = 1.2, 
  text.col = "black", 
  horiz = F , 
  inset = c(0.1, 0.1, 0.1))

dev.off()

# # plot 2

# df<-data.frame()

# ymax = 0

# # plot 3

# df<-data.frame()

# ymax = 0

# # plot rec 1

# df<-data.frame()

# ymax = 0

# # plot rec 2

# df<-data.frame()

# ymax = 0
