library(quantmod)
from.dat <- as.Date("01/01/08", format = "%m/%d/%y")
to.dat <- as.Date("12/31/13", format = "%m/%d/%y")
getSymbols("GOOG",
           src = "google",
           from = from.dat,
           to = to.dat)

head(GOOG)

mGoog <- to.monthly(GOOG[, -5])
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency = 12)
plot(ts1, xlab = "Years+1", ylab = "GOOG")

# Trend - Consistently increasing pattern over time
# Seasonal - When there is a pattern over a fixed period of time that recurs.
# Cyclic - When data rises and falls over non fixed periods

plot(decompose(ts1), xlab = "Years+1")

ts1Train <- window(ts1, start = 1, end = 5)
ts1Test <- window(ts1, start = 5, end = (7 - 0.01))
ts1Train

plot(ts1Train)
lines(ma(ts1Train, order = 3), col = "red") #not working

library(forecast)
ets1 <- ets(ts1Train, model = "MMM")
fcast <- forecast(ets1)
plot(fcast)
lines(ts1Test, col = "red")

accuracy(fcast, ts1Test)
