

i <- c(rep("A", 4), rep("B", 9), rep("C", 16))
y <- c(rep(2000:2001, each = 2), rep(2000:2002, each = 3), rep(2000:2003, each=4))
w <- c(rep(1:2, times = 2), rep(1:3, times = 3), rep(1:4, times = 4))
p <- runif(length(i), 0,1)
d <- data.frame(i, y, w, p)

arr <- tapply(d$p, d[ ,c(1,2,3) ], FUN = print,  default = 0 )
arr
tapply(d$p, d[ ,c(1,2,3) ], FUN = sum,  default = 0 )
