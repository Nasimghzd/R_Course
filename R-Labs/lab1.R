
# Lab: Introduction to R


## Basic Commands

### vectors, data, matrices, subsetting/indexing

# create a vector using function c() for concatenate
x <- c(1, 3, 2, 5)
x
is.vector(x)
y<-1:10
y
is.vector(y)
###
x=c(2,7,5)
x
y=seq(from=4,length=3,by=3)
?seq
y
length(x)
length(y)
ls()
# arithmetic operations with vectors are element-by-element
x+y
x/y
x^y
x[2]
x[2:3]
x[-2]
x[-c(1,2)]
rm(x, y)
?matrix
## by default the filling of a matrix is done by column (byrow=FALSE)
x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
### same as before
x <- matrix(c(1, 2, 3, 4), 2, 2)
x
### to fill a matrix by row we set byrow=TRUE
matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)
###
# arithmetic transformations of a matrix are element-by-element
sqrt(x)
x^2
# subsetting in matrices follow the same logic as in vectors
z=matrix(seq(1,12),4,3)
z
z[3:4,2:3]
# no indication of rows means "all rows"
z[,2:3]
is.matrix(z[,2:3])
# no indication of columns means "all columns"
z[2:3,]
z[-c(1, 3), ]
z[-c(1, 3), -c(1, 3)]
is.matrix(z[-c(1, 3), -c(1, 3)])
is.vector(z[-c(1, 3), -c(1, 3)])
# when we extract just one column or one row we should be careful
# as the nature of the outcome may change from matrix to vector
z[,1]
is.matrix(z[,1])
is.vector(z[,1])
z[,1,drop=FALSE]
is.matrix(z[,1, drop=FALSE])
# check also when a row is extracted
z[1,]
is.matrix(z[1,])
is.vector(z[1,])
z[1,,drop=FALSE]
is.matrix(z[1,,drop=FALSE])
#
dim(z)
class(z)
ls()
rm(x)
ls()
###########
###
###
###
### Generating random data, graphics
### rnorm(50) to generate 50 realizations of a standard normal rv (or N(0,1)) 
x <- rnorm(50)
x
hist(x)
y <- x + rnorm(50, mean = 50, sd = .1)
cor(x, y)
###
set.seed(1303)
rnorm(50)
###
set.seed(3)
y <- rnorm(100)
hist(y)
mean(y)
var(y)
sqrt(var(y))
sd(y)
#
### runif(50) to generate 50 realizations of a uniform(0,1) rv
x=runif(50)
y=rnorm(50)
plot(x,y)
plot(x,y,xlab="Random Uniform",ylab="Random Normal",main = "Plot of x vs y", pch="*",col="blue")
?plot
### how to save a figure. 
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()
# dev.off tells R to redirect output back to console
#
# how to align multiple graphs with parameter (par) mfrow=multiple figures
# mfrow=c(2,1): to put two graphs in one column
par(mfrow=c(2,1))
plot(x,y)
hist(y)
# mfrow=c(1,2): to put two graphs in one row
par(mfrow=c(1,2))
plot(x,y)
hist(y)
# return to one-graph at a time
par(mfrow=c(1,1))
# 
### Reading in data / Loading data
#
### Auto.data has data in ascii format
# read in data with function read.table()
# you can use write.table() when you want to export data in ascii file
getwd()
Auto <- read.table("Auto.data")
# Auto=read.table("../Auto.data") in case file is not in my working dir
View(Auto)
head(Auto)
dim(Auto) # 398 rows
# from View() and head() I see that the reading was not correct.
# the variable names were taken as data
###
Auto <- read.table("Auto.data", header = T, na.strings = "?", stringsAsFactors = T)
View(Auto)
dim(Auto) # 397 rows
class(Auto)
#
# more convenient to read in data from an excel file that has been saved as
# csv (comma-separated values) with function read.csv
Auto=read.csv("Auto.csv")
#
is.data.frame(Auto)
# Auto=read.csv("../Auto.csv")
View(Auto)
head(Auto)
# try
pairs(Auto,col="brown")  # it fails, WHY?
###
# compare with the following
Auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors = T)
pairs(Auto,col="brown")  # it works, WHY?
#
# stringsAsFactors = T tells R to interpret each variable containing 
# character strings as qualitative variable
# and each distinct character string represents a distinct level
#
# na.strings indicates which character should treated as missing obs
View(Auto)
Auto[1:4, ]
#
names(Auto)
dim(Auto)
class(Auto)
###
Auto <- na.omit(Auto)
dim(Auto)
###
summary(Auto)
# to use a variable in object Auto we use the $-sign 
#to subset/extract a variable
plot(Auto$cylinders,Auto$mpg)
plot(Auto$cyl,Auto$mpg)
# to simplify we can attach Auto and its components
# then we don't need the $-sign 
attach(Auto)
search()
ls(Auto, pos=2)
plot(cylinders,mpg)
plot(cylinders, mpg, col = "red")
# treat cylinders as qualitative
cylinders=as.factor(cylinders)
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
pdf(file="mpg.pdf")
#pdf(file="../mpg.pdf")
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
dev.off()
pairs(Auto,col="brown")
pdf(file="crosscor.pdf")
pairs(Auto,col="brown")
dev.off()
pairs(
  ~ mpg + displacement + horsepower + weight + acceleration,
  data = Auto
)
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T,
     horizontal = T)
plot(cylinders, mpg, col = "red", varwidth = T,
     xlab = "cylinders", ylab = "MPG")
###
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)
###
summary(mpg)
###
summary(name)
class(name)
nlevels(name)
q()