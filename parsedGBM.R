## creates data.frame of one year some stock, and make wkday col
filePath <- file.choose()
tmpFinal <- read.csv(filePath)

tmpFinal$Date <- as.Date(tmpFinal$Date, format="%m/%d/%Y")
tmpFinal$Weekday <- weekdays(tmpFinal$Date)

## create for to extract Fridays only recreates w/o loop
fridayOnly <- tmpFinal[tmpFinal$Weekday == "Friday",]

## one thing to add, reverse order to have oldest to newest
fridayOnly <- fridayOnly[nrow(fridayOnly):1, ]

## Change from orig \\$ to [$,] to account for , on top of $
fridayOnly$Close.Last <- as.numeric(gsub("[$,]","",fridayOnly$Close.Last))

## One liner for loop previously % per week, adds col named ratio_rev in orig
percentChange <- fridayOnly$Close.Last[-1] / fridayOnly$Close.Last[-nrow(fridayOnly)]
fridayOnly$percentChan <- c(NA,percentChange)

## On how to delete cols deleting useless col
fridayOnly$Weekday <- NULL

## Now log ratios
logRatios <- numeric(nrow(fridayOnly))

## log of ratios
fridayOnly$logRatios <- log(fridayOnly$percentChan)

## Build log_sq
## logSq <- fridayOnly$logRatios ** 2

## Now to create the model
Traj <- ts(fridayOnly$Close.Last)
stock.rtn = diff(log(Traj))
returns = as.vector(stock.rtn)
dt = 1.0/dim(as.matrix(Traj))[1]
r_bar = mean(returns)
s_r = sd(returns)
mu_hat = r_bar/dt + (s_r^2)/(2*dt)
sigma_hat = s_r/sqrt(dt)
X0 = Traj[1]
T = 1 ## length
nt = 50 ## number of paths
n = nrow(fridayOnly) ## number of fridays
t = (0:n) * dt
X = matrix(rep(0,length(t)*nt),nrow=nt)

GBM <- function(x, r, sigma_hat, T, N) {
  dt <- T / N ## distance per time step
  W <- c(0, cumsum(rnorm(N, mean = 0, sd = sqrt(dt)))) ## weiner process
  X <- x * exp((r - 0.5 * sigma_hat^2) * (0:N) * dt  + sigma_hat * W) ## S_t
  return(X)
}

for(i in 1:nt){
  X[i,] = GBM(x=X0,r=mu_hat,sigma_hat,T=T,N=n)
}

ymax <- max(X)
ymin <- min(X)
## check pdf, recreates mu_h and sigma_hat to plot GBM

## Now plot
plot(t,X[1,],t='l',ylim=c(ymin,ymax),col=1,ylab="Price X(t)",xlab="time")
for(i in 2:nt){
  lines(t,X[i,],t='l',ylim=c(ymin,ymax),col=i)
}

## mape to measure accuracy
mean_path <- colMeans(X)
sim <- mean_path[-1]
actual <- fridayOnly$Close.Last

mape <- mean(abs(sim - actual) / actual) * 100
accuracy <- 100 - mape

print(paste0("GBM sim compared with real historical: ", round(accuracy, 2), "%"))

## multiple trials
n_trials <- 100
accuracies <- numeric(n_trials)

for(i in 1:n_trials){
  X <- matrix(rep(0,length(t)*nt), nrow=nt)
  for(j in 1:nt){
    X[i,] <- GBM(x=X0, r=mu_hat, sigma_hat, T=T, N=n)
  }
  mean_path <- colMeans(X)
  sim <- mean_path[-1]
  actual <- fridayOnly$Close.Last
  mape <- mean(abs(sim - actual)/actual) * 100
  accuracies[k] <- 100 - mape
}

mean_acc <- mean(accuracies)
sd_acc <- sd(accuracies)
print(paste0("Mean of above for", n_trials, ": ", round(mean_acc,2), "% +- ", round(sd_acc,2), "%"))
