# comment:
# script file to test available ADF methods:
# manually: by fitting lm() on ADF regression
# using tseries::adf.test()
# using urca::ur.df()


# results so far:
# adf.test vs manual: 
#   adf.test seems to reject unit root more often 
#   (based on 10 manual comparison)

# ur.df() vs manual:
#   ur.df() also seems to use lm() internally,
#   ur.df( ..., type = "none") calls lm() without intercept: 
#     lm( cx ~ 0 + x_1 + cx_1 + cx_2 + ...)
#   ur.df( ..., type = "drift") calls lm() with intercept: e.g.
#     lm( cx ~ x_1 + cx_1 + cx_2 + ...)
#   ur.df( ..., type = "time") calls lm() with a time trendL e.g. 
#     lm( cx ~ t + x_1 + cx_1 + cx_2 + ...)
#   therefore results are the same.
#   can see by using summary(ur.df(...))

require(tseries)
require(urca)



# times where lm rejects more: 
# x

# times where adf.test rejects more: 
# xxxx



# testing function adf.test()
(x <- rnorm(1000))  # no unit-root

x_1 <- c(NA,x[1:length(x)-1])
cx <- x - x_1
cx_1 <- c(NA, cx[1:length(cx)-1])
cx_2 <- c(NA, cx_1[1:length(cx_1)-1])
cx_3 <- c(NA, cx_2[1:length(cx_2)-1])
cx_4 <- c(NA, cx_3[1:length(cx_3)-1])

print(" Lag length 0 ----------------------------------------------------------")
# ADF test with lag length 1
# manual
print(summary(lm(cx ~ x_1)))
# using adf.test
print(adf.test(x, k = 0))
print(ur.df(x, lags = 0))

print(" Lag length 1 ----------------------------------------------------------")
# ADF test with lag length 1
# manual
print(summary(lm(cx ~ x_1 + cx_1)))
# using adf.test
print(adf.test(x, k = 1))
print(ur.df(x, lags = 1))

print(" Lag length 2 ----------------------------------------------------------")
# ADF test with lag length 1
# manual
print(summary(lm(cx ~ x_1 + cx_1 + cx_2)))
# using adf.test
print(adf.test(x, k = 2))
print(ur.df(x, lags = 2))


print(" Lag length 3 ----------------------------------------------------------")
# ADF test with lag length 1
# manual
print(summary(lm(cx ~ x_1 + cx_1 + cx_2 + cx_3)))
# using adf.test
print(adf.test(x, k = 3))
print(ur.df(x, lags = 3))

print(" Lag length 4 ----------------------------------------------------------")
# ADF test with lag length 1
# manual
t <- 1:length(x)
print(summary(lm(cx ~ t+ x_1 + cx_1 + cx_2 + cx_3 + cx_4)))
# using adf.test
print(adf.test(x, k = 4))
print(summary(ur.df(x, type = "trend", lags = 4)))
