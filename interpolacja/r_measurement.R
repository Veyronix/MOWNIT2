library(ggplot2)
library(readr)
library(int)
newton <- read_csv("newtonMeasurement.csv")
lagrange <- read_csv("lagrangeMeasurement.csv")
julia <- read_csv("juliaInterpolation.csv")
linear <- read_csv("linearInterpolation.csv")

x = c(7.81515229336533, 2.190433419226773, 1.1166175860847705, 8.728457202028592, 6.194745088080948, 0.5681528278618153, 2.4791591999355456, 3.3069387478169565, 3.3866802981361155, 8.132015199706576, 8.103630107687096)  
y = c(0.10958626314134179, 0.13110494656767369, 0.6191655900511863, 0.3131654005552298, 0.030052013487595453, 0.5967652898565929, 0.8223881329130259, 0.1555084681715152, 0.5672714149930127, 0.1586610303192849, 0.8130387494143255)
mydata = data.frame(x,y)

ggplot(mydata,aes(x,y)) + geom_point()+ ylim(-30,43) + xlim(0,10)
last_plot() + geom_line(data=newton, aes(x,y))

ggplot(mydata,aes(x,y)) + geom_point()
last_plot() + geom_line(data=lagrange, aes(x,y))

linear$metoda <- "linear spline"
julia_tmp <- julia
julia_tmp$metoda <- "Polynomials" 
linear_new <- merge(julia_tmp,linear,all=TRUE)


ggplot(mydata,aes(x,y)) + geom_point()
last_plot() + geom_line(data=julia, aes(x,y),color='red')
last_plot() + geom_line(data=linear, aes(x,y),color='green')

newton_times <- read_csv("newtonTimes.csv")
newton_times_result = aggregate( time ~ n, data=newton_times, FUN=mean)
newton_times_result$sd = aggregate( time ~ n, data=newton_times, FUN=sd)$time
newton_times_result$metoda <- "newton"

lagrange_times <- read_csv("lagrangeTimes.csv")
lagrange_times_result = aggregate( time ~ n, data=lagrange_times, FUN=mean)
lagrange_times_result$sd = aggregate( time ~ n, data=lagrange_times, FUN=sd)$time
lagrange_times_result$metoda <- "lagrange"

julia_times <- read_csv("juliaTimes.csv")
julia_times_result = aggregate( time ~ n, data=julia_times, FUN=mean)
julia_times_result$sd = aggregate( time ~ n, data=julia_times, FUN=sd)$time
julia_times_result$metoda <- "julia"

all_results <- merge(newton_times_result,lagrange_times_result, all=TRUE)
all_results <- merge(all_results,julia_times_result,all=TRUE)

ggplot(all_results, aes(n,time)) + geom_point(aes(colour=metoda),size=3) + xlim(45,652) + ylim(0,1000) +
  geom_errorbar(aes(ymin=time-sd, ymax=time+sd), width=.1) +
  xlab("ilość punktów") + ylab("czas [ms]") 


newton_fit = lm(time ~ poly(n, 3, raw=TRUE), data=newton_times_result)
lagrange_fit = lm(time ~ poly(n, 3, raw=TRUE), data=lagrange_times_result)
julia_fit = lm(time ~ poly(n, 3, raw=TRUE), data=julia_times_result)


newtow_newdata = data.frame(n = seq(0,700, length.out=1000))
newtow_newdata$time = predict(newton_fit, newtow_newdata)
last_plot() + geom_line(data=newtow_newdata, aes(n,time),color='blue')


lagrange_newdata = data.frame(n = seq(0,700, length.out=1000))
lagrange_newdata$time = predict(lagrange_fit, lagrange_newdata)
last_plot() + geom_line(data=lagrange_newdata, aes(n,time),color='green')

julia_newdata = data.frame(n = seq(0,700, length.out=1000))
julia_newdata$time = predict(julia_fit, julia_newdata)
last_plot() + geom_line(data=julia_newdata, aes(n,time),color='red')



pp <- cubicspline(x, y)
ppfun <- function(xs) ppval(pp, xs)


