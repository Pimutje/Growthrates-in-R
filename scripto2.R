##ouvrir dossier type Rworkspace##
##ouvrir script##


library(growthrates)
library(ggplot2)
library(tidyr)
library(tidyverse)


FINAL <- read.csv2("C:\\Users\\Pimut\\OneDrive\\UvA\\Lab\\Results\\Spent medium growth\\Data2.csv")


str(FINAL)
colnames(FINAL)



df_wide <- FINAL %>% pivot_longer(c(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54, X55, X56, X57, X58, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77, X78, X79, X80, X81, X82, X83, X84, X85, X86, X87, X88, X89, X90, X91, X92, X93, X94, X95, X96), names_to = "colname", values_to = "val")

ggplot(df_wide, aes(x = Time..h. ,value, y=val, color=colname)) + geom_point()
t <- FINAL$Time..h.

fitfun <- function(x){coef(fit_easylinear(t, x))[3]}

fitfun(FINAL$X84)

result <- sapply(FINAL[,-1],fitfun)

write.csv(result,"C:\\Users\\Pimut\\OneDrive\\UvA\\Lab\\Results\\Spent medium growth\\growthrates.csv")

fit <- fit_easylinear(t, FINAL$X84)
summary(fit)
coef(fit)
rsquared(fit)
deviance(fit)
par(mfrow = c(1, 2))
plot(fit, log = "y")
plot(fit)
