library('psych')

data <- read.csv("/home/home/source.csv", sep=",", header=TRUE)
cor(data)
cortest.barlett(cor(data))

set.seed(42)
x <- matrix(rnorm(1000),ncol=10)
r <- cor(x)
cortest.bartlett(r)
#random data don’t differ from an identity matrix
data(bfi)
cortest.bartlett(bfi)
#not an identity matrix
