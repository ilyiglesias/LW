# an alternative way to plot these length weight values via nls

#NOTE: I was also able to plot these values via nls (non-linear least squares) in case I need to do this again!!
# nls- non-linear regression model- an alternative way to estimate parameters #######################
# i followed instructions from https://stackoverflow.com/questions/22664763/plotting-a-power-fit-of-for-y-axb
m <- nls(Weight~a*Std_Length^b, data = lw, start = list(a=1, b=1))
summary(m) # this reveals totally different values for intercept: 0.000004187 and slope: 3.29
coef(m)
# estimate goodness of fit? Not sure thorough what methods
cor(lw$Std_Length, predict(m)) # 0.85 so I think this is fairly decent

plot(lw$Std_Length, lw$Weight, xlab="Fish Length (mm)", ylab= "Fish Weight (g)") # this is a plot of length (x) and weight (y)
x_L <- sort(lw$Std_Length) # this sorts the length data- not sure why i need this but without doing this step I get a very strange zig zag pattern
lines(x_L, predict(m, list(Std_Length=x_L)), col="purple", lwd=4) # this plots the line for our x= x_L lengths and predicted y (weight) values based on our estimated parameters from the nls model
