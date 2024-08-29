
## test codes

## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)

c(1,2)


# exercise ----------------------------------------------------------------

## vector

x <- c(1, 3, 4, 8)
x

## ex. 1.a characters
x <- c("a", "b", "c")
x

## ex. 1c logical
x <- c("TRUE", "FALSE", "FALSE")
x

## ex. 2 sequence of numbers
x <- 1:5
x

## ex 3a replicate same number of characters
x <- rep(2,5)
x

## ex 3b replicate same numbers or characters
x <- rep("a", 5)
x

## ex 4b use seq() function
x <- seq(1,5, by =0.1)
x

## ex 4c use seq() function
x <- seq(1, 5, length = 7)
x

##check features
#numeric
x <- c(1.2, 3.1, 4.0, 8.2)
x
class(x)
typeof(x)
length(x)
sum(x)
mean(x) #gives arithmetic mean 

#character vector 
y <- c("a", "b", "c")
class(y)
length(y)

##element ID
x <- c(2,2,3,2,5)

#element 2
x[2]

#element 2 and 4
x[c(2,4)]

#element 2 through 4
x[2:4]


## Matrix
# ex 1 cbind
x <- cbind(c(1,2,3), c(4,5,6))
x

#ex 2 rbind
x <- rbind(c(1,2,3), c(4,5,6))
x

#ex 3 matrix: specify elements and the number of rows (nrow) and columns (ncol)
x <- matrix(1:9, nrow = 3, ncol = 3)
x <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
x

dim(x)
rowSums(x)
colSums(x)

#access 
x <- matrix(1:9, nrow = 3, ncol = 3)
x

x[2,3] # access and element in row #2 and column #3

x[2,] #access elements in row #2

x[c(2,3), ] #access elements in rows #2 and 3

x[, c(2,3)] #access elements in columns #2 and 3

## Data frames
# Create data frame
x <- c("Pristine", "Pristine", "Disturbed", "Disturbed", "Pristine") # Lake type
y <- c(1.2, 2.2, 10.9, 50.0, 3.0) # TSS: total suspended solids (mg/L)
df0 <- data.frame(LakeType = x, TSS = y) # x is named as "LakeType" while y is named as "TSS"
df0

#call column names
colnames(df0)

#Access by columns 
df0$LakeType

df0$TSS

df0[,1]

df0[1,]

df0[c(2,4),]

