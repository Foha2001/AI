library("fpp")
library("e1071")
beer_train = head(ausbeer, -20)
beer_test = head(tail(ausbeer, 20), 10)

library("mltsp")

spec = build_narx(svm, p=2, d=0, P=1, D=1, freq=frequency(ausbeer))
model = narx(spec, beer_train)

model


fcst = forecast(model, h = 10)
plot(fcst)
lines(beer_test, col="red")


set.seed(0)

tstamps = seq(as.Date("2000-01-01"), length.out = 110, by='day')
x = xts(runif(length(tstamps)), tstamps)
xreg = 1 - 0.5 * x
yreg = xts(runif(110), tstamps)

colnames(xreg) = colnames(yreg) = "xreg"

# training and testing data
x_train = head(x, 100)
x_test = tail(x, 10)
ind_test = index(x_test)

model = narx(x_train, SimpleLM, p = 2)
pred1 = forecast(model, h=10)

model2 = narx(x_train, sigmoid, p = 2, xreg = xreg)
pred2 = forecast(model2, xreg=xreg[ind_test])

model3 = narx(x_train, SimpleLM, p = 2, xreg = yreg)
pred3 = forecast(model3, xreg=yreg[ind_test])

rmse <- function(x,y) sqrt(mean((x-y)^2))

c(Err_without_xreg= rmse(pred1$mean, x_test),
  Err_with_xreg= rmse(pred2$mean, x_test),
  Err_with_bad_xreg= rmse(pred3$mean, x_test))










