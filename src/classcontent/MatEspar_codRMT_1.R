
########################################################################
########################################################################
### Using sparse matrix in R
########################################################################
########################################################################

# 3 main packages: Matrix, slam and glmnet

########################################################################
### The Matrix package
########################################################################

### Main characteristics

# For only 0 matrix

library('Matrix')

m0 <- matrix(0, nrow = 1000, ncol = 1000)
m1 <- Matrix(0, nrow = 1000, ncol = 1000, sparse = FALSE)
m2 <- Matrix(0, nrow = 1000, ncol = 1000, sparse = TRUE)


object.size(m0)
object.size(m1)

object.size(m2)

# Puting one element <> 0

m1[500, 500] <- 1
m2[500, 500] <- 1

object.size(m1)

object.size(m2)

# Realize for the case above that for m2 the size has this formula:
# 5632 + 16 * (# elements <> 0). 
# If all elements are <> 0, then the size of the sparse matrix is 
# 5632 + 16 * 1000 * 1000 = 16.005.632 bytes (almost 2X the original matrix).

### You can make matrices operations with sparse matrices normally

m2a <- Matrix(0, nrow = 10, ncol = 10, sparse = TRUE)
m2a[5, 5] <- 1

m2a %*% rnorm(10)
m2a + m2a
m2a - m2a
t(m2a)

m3 <- cBind(m2a, m2a)
m3
nrow(m3)
ncol(m3)

m4 <- rBind(m2a, m2a)
m4
nrow(m4)
ncol(m4)

########################################################################
### The slam package
########################################################################

# It is an alternative. It is lighter than the Matrix package. Look:

#install.packages('slam')
library('slam')

m1 <- Matrix(0, nrow = 1000, ncol = 1000)
m2 <- simple_triplet_zero_matrix(nrow = 1000, ncol = 1000)

object.size(m1)

object.size(m2)

# Puting one element <> 0

m1[500, 500] <- 1
m2[500, 500] <- 1

object.size(m1)
object.size(m2)

# Realize for the case above that for m2 the size has this formula:
# 1032 + 24 * (# elements <> 0). 
# If all elements are <> 0, then the size of the sparse matrix is 
# 1032 + 24 * 1000 * 1000.

### You can make matrices operations with sparse matrices normally

m2a <- simple_triplet_zero_matrix(nrow = 10, ncol = 10)
m2a[5, 5] <- 1

m2 %*% rnorm(10)
m2 + m2
m2 - m2
t(m2)

########################################################################
### The glmnet package
########################################################################

# This is not a sparse matrix package, but it uses the Matrix package to 
# operate GLM models. Than we can use this library to measure performance.

library('Matrix')
#install.packages('glmnet')
library('glmnet')

n <- 10000
p <- 500

x <- matrix(rnorm(n * p), n, p)
iz <- sample(1:(n * p),
             size = n * p * 0.85,
             replace = FALSE)
x[iz] <- 0

object.size(x)

sx <- Matrix(x, sparse = TRUE)

object.size(sx)

beta <- rnorm(p)

y <- x %*% beta + rnorm(n)

glmnet.fit <- glmnet(x, y)

# How much more efficient is the use of sparse matrices?

library('Matrix')
library('glmnet')

set.seed(1)
performance <- data.frame()

for (sim in 1:10)
{
    n <- 10000
    p <- 500
    
    nzc <- trunc(p / 10)
    
    x <- matrix(rnorm(n * p), n, p)
    iz <- sample(1:(n * p),
                 size = n * p * 0.85,
                 replace = FALSE)
    x[iz] <- 0
    sx <- Matrix(x, sparse = TRUE)
    
    beta <- rnorm(nzc)
    fx <- x[, seq(nzc)] %*% beta
    
    eps <- rnorm(n)
    y <- fx + eps
    
    sparse.times <- system.time(fit1 <- glmnet(sx, y))
    full.times <- system.time(fit2 <- glmnet(x, y))
    sparse.size <- as.numeric(object.size(sx))
    full.size <- as.numeric(object.size(x))
    
    performance <- rbind(performance, data.frame(Format = 'Sparse',
                                                 UserTime = sparse.times[1],
                                                 SystemTime = sparse.times[2],
                                                 ElapsedTime = sparse.times[3],
                                                 Size = sparse.size))
    performance <- rbind(performance, data.frame(Format = 'Full',
                                                 UserTime = full.times[1],
                                                 SystemTime = full.times[2],
                                                 ElapsedTime = full.times[3],
                                                 Size = full.size))
}

#install.packages('Hmisc')
library(Hmisc)
#install.packages('ggplot2')
library(ggplot2)

ggplot(performance, aes(x = Format, y = UserTime, fill = Format)) +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'bar') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
    ylab('User Time in Seconds') #+
    #opts(legend.position = 'none')
#ggsave('sparse_vs_full_user_time.pdf')

ggplot(performance, aes(x = Format, y = SystemTime, fill = Format)) +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'bar') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
    ylab('System Time in Seconds') #+
    #opts(legend.position = 'none')
#ggsave('sparse_vs_full_system_time.pdf')

ggplot(performance, aes(x = Format, y = ElapsedTime, fill = Format)) +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'bar') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
    ylab('Elapsed Time in Seconds') #+
    #opts(legend.position = 'none')
#ggsave('sparse_vs_full_elapsed_time.pdf')

ggplot(performance, aes(x = Format, y = Size / 1000000, fill = Format)) +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'bar') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') #+
    ylab('Matrix Size in MB') +
    #opts(legend.position = 'none')
#ggsave('sparse_vs_full_memory.pdf')