# setup -------------------------------------------------------------------
rm(list = ls())

library(tidyverse)

# central tendency --------------------------------------------------------

## central tendency uses arithmetic mean, geometric mean, and median
## typically average or mean refers to arithmetic mean
## geometric mean (for xi = 3) = (x1*x2*x3)^1/3

## exercise 1.1.2
# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

x
y

# arithmetic means for x and y
a_x <- mean(x)
a_y <- mean(y)
## or
sum(y)/length(y)

# for vector x
n_x <- length(x) # the number of elements in x = the number of data points
sum_x <- sum(x) # summation for x
mu_x <- sum_x / n_x # arithmetic mean
print(mu_x) # print calculated value

# for vector y; we can calculate directly too
mu_y <- sum(y) / length(y)
print(mu_y) # print calculated value

# geometric mean for x and y
# for x
prod_x <- prod(x)
n_x <- length(x)
mug_x <- prod_x^(1/n_x)
print(mug_x)

# for y
mug_y = prod(y)^(1/length(y))
print(mug_y)

## or take arithmetic mean in log scale and transform back to ordinary scale\
log_y <- log(y)
exp(mean(log_y))

# comparison of means
print(a_x)
print(mug_x) #aritmetic mean will always be larger - highly influenced by outliers

# median for x and y
# for x
median(x)

x = sort(x)
index <- (length(x)+1)/ 2 
med_x <- x[index]
print(med_x)

#for y
median(y)

y <- sort(y)
med_y <- y[(length(y) + 1) / 2]
print(med_y)


# variation ---------------------------------------------------------------

## variance calculates the spread or diversion of the dataset
## standard deviation (SD) is the square root of variance 
## IQR and MAD are less susceptible to outliers as SD and variance
## variance = (sum(x - arith. mean)^2)/N

## exercise 1.2.2 

## calculate variance for x and y manually
var_x <- sum((x - mean(x))^2) / length(x)
print(var_x)

var_y <- sum((y-mean(y))^2) / length(y)
print(var_y)

sd_x <- sqrt(var_x)
sd_y <- sqrt(var_y)

## quantile range
## quantile()
x25 <- quantile(x, 0.25)
x75 <- quantile(x, 0.75)
iqr_x <- abs(x25 - x75)
print(iqr_x)

y_q <- quantile(y, c(0.25, 0.75))
iqr_y <- abs(y_q[1] - y_q[2])
print(iqr_y)

## mad
ad_x <- abs(x-median(x))
mad_x <- median(ad_x)
print(mad_x)

mad_y <- median(abs(y-median(y)))
print(mad_y)


# relative variance -------------------------------------------------------

##coefficien of variation
## cv for x and y
## sd / mean

cv_x <- sd_x / mean(x) 
cv_y <- sd_y / mean(y)

## IQR / median
(iqmed_x <- iqr_x / median(x))
(iqmed_y <- iqr_y / median(y))
