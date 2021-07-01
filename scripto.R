##ouvrir dossier type Rworkspace##
##ouvrir script##


library(growthrates)
library(ggplot2)
library(tidyr)

FINAL <- read.csv2("C:/Users/Pim/OneDrive/UvA/Lab/Results/Spent medium growth/FINAL.csv")
str(FINAL)

 df_wide <- FINAL %>% pivot_longer(c(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54, X55, X56, X57, X58, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77, X78, X79, X80, X81, X82, X83, X84), names_to = "colname", values_to = "val")

 
  ggplot(df_wide, aes(x = Time..h. ,value, y=val, color=colname)) + geom_point()
t <- FINAL$Time..h.

fitfun <- function(x){coef(fit_easylinear(t, x))[3]}
result <- sapply(FINAL[,-1],fitfun)
graphs <- 
  
  
fit <- fit_easylinear(t, FINAL$X84)
par(mfrow = c(1, 2))
plot(fit, log = "y")
plot(fit)

write.csv(result,"C:\\Users\\Pimut\\OneDrive\\UvA\\Lab\\Results\\Spent medium growth\\growthrates.csv")


fit <- fit_easylinear(t, FINAL$X84)
summary(fit)
coef(fit)
rsquared(fit)
deviance(fit)

##plot(average.time.h , Amean , type="l" , col="green")##
##lines(average.time.h , C1mean , type="l" , col="blue")##

par(mfrow = c(1, 2))
plot(fit, log = "y")
plot(fit)

