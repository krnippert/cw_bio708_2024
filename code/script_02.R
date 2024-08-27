

# setup -------------------------------------------------------------------

rm(list = ls())

# exercise 1 - vector -----------------------------------------------------

#numeric vectors
nv1 <- rep(3,3)
nv1

nv2 <- seq(1,3.5, by =0.5)
nv2

nv3 <- 21:40
nv3

#character vectors
cv1 <- c("a","b","c")
cv1

cv2 <- rep("k",6)
cv2

cv3 <- letters[1:20]
cv3  
length(cv3)

set.seed(1)
x <- rnorm(100)

##element ID
which(x>2.0) 

##element values
x[x>2.0] #returns element values


# exercise 2 - matrix -----------------------------------------------------

#numeric matrix
nm1 <- cbind(rep(1,4),
            rep(2,4),
            rep(3,4),
            rep(4,4))
nm1

nm2 <- rbind(rep(1,4),
            rep(2,4),
            rep(3,4),
            rep(4,4))
nm2

#character matrix
cm3 <- cbind(rep("a",4),
             rep("b",4),
             rep("c",4),
             rep("d",4))
cm3

cm4 <- rbind(rep("a",4),
            rep("b",4),
            rep("c",4),
            rep("d",4))
cm4

#random
set.seed(1)
x <- matrix(rnorm(100), nrow = 10, ncol = 10)

#element id
which(x > 2,0, arr.ind=TRUE)

#element values
x[x>2]

#mean
mean(x[x>2])

# exercise 3 - data frame -------------------------------------------------

#creating data frame
x <- c("graze","ungraze", 
       "graze", "ungraze",
       "ungraze","ungraze",
       "graze","graze",
       "graze","graze")

y <- c(rep(1:5, each = 2))
y

z <- c(seq(25,29.5, by=0.5))
length(z)

df1 <- data.frame(treatment = x, biomass =y, temp = z)
df1

#checking data structure
colnames(df1)
df1$treatment
df1$biomass
df1$temp

#random
set.seed(1)
x <- rnorm(100, mean = 10, sd = 3)
y <- rpois(100, lambda = 10)
z <- rep(c("VA", "NC"), 50)
df0 <- data.frame(temperature = x, abundance = y, state = z)

df0

#calculating mean
v_va <- df0$temperature[df0$state =="VA"]
v_nc <- df0$temperature[df0$state =="NC"]

m_va <- mean(v_va)
m_nc <- mean(v_nc)

a_va <- mean(df0$abundance[df0$state == "VA"])
a_va

a_nc <- mean(df0$abundance[df0$state == "NC"])
a_nc

## extra exercise
v_va <- with(df0, temperature[state =="VA"])
v_nc <- with(df0, temperature[state =="NC"])

## extra exercise - alternative way tapply()
v_mu <- tapply(df0$temperature, 
       INDEX = df0$state,
       FUN = mean)

v_mu <- with(df0, 
        tapply(temperature,
            INDEX = state,
            FUN=mean))
