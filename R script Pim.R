##ouvrir dossier type Rworkspace##
##ouvrir script##


library(growthrates)
library(ggplot2)
library(tidyr)

FINAL <- read.csv2("C:/Users/Pimut/OneDrive/UvA/Lab/Results/Spent medium growth/Run 2/FINAL.csv")
str(FINAL)

df_wide <- FINAL %>% pivot_longer(c(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54, X55, X56, X57, X58, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77, X78, X79, X80, X81, X82, X83, X84, X85, X86, X87, X88, X89, X90, X91, X92, X93, X94, X95, X96), names_to = "colname", values_to = "val")

 
ggplot(df_wide, aes(x = Time..h. ,value, y=val, color=colname)) + geom_point()
t <- FINAL$Time..h.

#function to get the growthrates
fitfun <- function(x){coef(fit_easylinear(t, x))[3]}
result <- sapply(FINAL[,-1],fitfun)
write.csv(result,"C:\\Users\\Pimut\\OneDrive\\UvA\\Lab\\Results\\Spent medium growth\\Run 2\\growthrates.csv")


#test van Casper
i <- 1
while (i < 97) {
  #Initialize variable
  data_column <- paste(FINAL$X, i, sep="")
  
  #Run script
  fit <- fit_easylinear(t, data_column)
  par(mfrow = c(1, 2))		
  plot(fit, log = "y")		
  plot(fit)	
  
  #Increment column
  i <- i + 1
}

  
#Making the plots for X1 to X86
fit <- fit_easylinear(t,	 FINAL$X1	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X2	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X3	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X4	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X5	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X6	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X7	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X8	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X9	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X10	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X11	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X12	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X13	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X14	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X15	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X16	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X17	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X18	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X19	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X20	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X21	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X22	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X23	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X24	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X25	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X26	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X27	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X28	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X29	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X30	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X31	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X32	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X33	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X34	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X35	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X36	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X37	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X38	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X39	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X40	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X41	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X42	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X43	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X44	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X45	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X46	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X47	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X48	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X49	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X50	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X51	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X52	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X53	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X54	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X55	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X56	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X57	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X58	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X59	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X60	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X61	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X62	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X63	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X64	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X65	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X66	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X67	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X68	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X69	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X70	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X71	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X72	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X73	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X74	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X75	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X76	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X77	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X78	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X79	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X80	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X81	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X82	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X83	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X84	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X85	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X86	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X87	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X88	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X89	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X90	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X91	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X92	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X93	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X94	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X95	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X96	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X97	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X98	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X99	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X100	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X101	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X102	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X103	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X104	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X105	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X106	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X107	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X108	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X109	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X110	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		
fit <- fit_easylinear(t,	 FINAL$X111	)
par(mfrow = c(1, 2))		
plot(fit, log = "y")		
plot(fit)		

#Saving all the plots
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:\\Users\\Pimut\\OneDrive\\UvA\\Lab\\Results\\Spent medium growth\\rplots")





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

