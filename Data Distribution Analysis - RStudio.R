#Load Libraries

library(lattice)
library(psych)
library(gmodels)
library(ggplot2)
library(cowplot)

carsDB

#Construct a scatterplot with smoothing for mpg vs. disp
ggplot(data = carsDB,aes(x = disp, y = mpg)) + geom_point(color='blue')+ stat_smooth(color='red',method = 'lm')+labs(title="Scatter Plot Miles per gallon(US) vs. Displacement (cu.in.) with Smoothing")+theme(plot.title = element_text(hjust=0.5)) + xlab('Displacement (cu.in.)') + ylab('Miles per gallon(US)')+ geom_smooth(color= 'red')


#Construct a scatterplot with smoothing for mpg vs. disp
ggplot(data = carsDB,aes(x = disp, y = mpg)) + geom_point(color='blue')+labs(title="Scatter Plot Miles per gallon(US) vs. Displacement (cu.in.) with Smoothing")+theme(plot.title = element_text(hjust=0.5)) + xlab('Displacement (cu.in.)') + ylab('Miles per gallon(US)')+ geom_smooth(color= 'red')

# TEST Construct a scatterplot with smoothing for mpg vs. disp
ggplot(data = carsDB,aes(x = disp, y = mpg)) + geom_point(color='blue')+ stat_smooth(color='Red',se = FALSE, method = "lm")+labs(title="Scatter Plot Miles per gallon(US) vs. Displacement (cu.in.) with Smoothing")+theme(plot.title = element_text(hjust=0.5)) + xlab('Displacement (cu.in.)') + ylab('Miles per gallon(US)')


#BoxPlot mpg w.r.t cyl
ggplot(data=carsDB, aes(factor(cyl), mpg))+ geom_boxplot(aes(fill = factor(cyl))) +
  labs(title="Boxplot Cyl vs MPG")+ theme(plot.title = element_text(hjust=0.5))



#Construct a scatterplot with smoothing for mpg vs. hp
ggplot(data = carsDB,aes(x = hp, y = mpg)) + geom_point()+ stat_smooth(method = 'lm')+labs(title="Scatter Plot Miles per gallon(US) vs. Gross horsepower with Smoothing")+theme(plot.title = element_text(hjust=0.5)) + xlab('Gross horsepower') + ylab('Miles per gallon(US)')

#Construct a scatterplot with smoothing for mpg vs. wt
ggplot(data = carsDB,aes(x = wt, y = mpg)) + geom_point()+ stat_smooth(method = 'lm')+labs(title="Scatter Plot Miles per gallon(US) vs. Weight (1000 lbs)r with Smoothing")+theme(plot.title = element_text(hjust=0.5)) + xlab('Weight (1000 lbs)') + ylab('Miles per gallon(US)')

#mpg vs hp
ggplot(data=carsDB,aes(x=mpg,y=hp,col=factor(cyl)))+geom_point()+labs(title="Scatter Plot
MPG and HP")+ theme(plot.title = element_text(hjust=0.5))

ggplot(data= carsDB, aes(x = hp, y = mpg,col=factor(cyl)))+geom_point() + geom_smooth(color = "Yellow") + labs(title = "Relationship between Efficiency of cars and Gross Horsepower ", x = "Gross Horsepower" ,y = "miles/(US) gallon (Fuel Efficiency)",color = "Number of Cylinders")

ggplot(data=carsDB,aes(x=mpg,y=hp,col=factor(cyl)))+geom_point()+labs(title="Scatter Plot
MPG and HP")+ theme(plot.title = element_text(hjust=0.5))

# plot with both points and smoothed line
ggplot(data=carsDB, aes(x = disp, y = mpg)) + geom_point(color='blue') + geom_smooth(color='red')+labs(title="Scatter Plot Miles per gallon(US) vs. Displacement (cu.in.) with Smoothing")+theme(plot.title = element_text(hjust=0.5)) + xlab('Displacement (cu.in.)') + ylab('Miles per gallon(US)')

#Corrolation between mpg and disp
cor(carsDB$mpg,carsDB$disp, use="complete.obs")

# plot with both points and smoothed line
ggplot(data=carsDB, aes(x = hp, y = mpg)) + geom_point(color='blue') + geom_smooth(color='red')+labs(title="Scatter Plot Miles per gallon(US) vs.Gross Hosepower (cu.in.) with Smoothing")+theme(plot.title = element_text(hjust=0.5)) + xlab('Gross Horsepower') + ylab('Miles per gallon(US)')

#Corrolation between mpg and hp
cor(carsDB$mpg,carsDB$hp, use="complete.obs")

# plot with both points and smoothed line
ggplot(data=carsDB, aes(x = wt, y = mpg)) + geom_point(color='blue') + geom_smooth(color='red', method="loess")+labs(title="Scatter Plot Miles per gallon(US) vs.Gross Hosepower (cu.in.) with Smoothing")+theme(plot.title = element_text(hjust=0.5)) + xlab('Gross Horsepower') + ylab('Miles per gallon(US)')

#Corrolation between mpg and hp
cor(carsDB$mpg,carsDB$wt, use="complete.obs")




#To plot a scatterplot with smoothing for Fuel Efficiency (Miles/gallon (US)) vs. Engine Displacement (cu. in.)
ggplot(data= carsDB, aes(x = disp, y = mpg,col=factor(cyl)))+geom_point() + geom_smooth(color = "yellow") + labs(title = "Relationship between Fuel Efficiency (Miles/gallon (US)) vs. Engine Displacement (cu. in.))", x = "Engine Displacement(cu. in.)" ,y = "Fuel Efficiency(miles/(US) gallon)",color = "Number of Cylinders")

#Corrolation between mpg and disp
cor(carsDB$mpg,carsDB$disp, use="complete.obs")

#To plot a scatterplot with smoothing for Fuel Efficiency(Miles/gallon(US)) vs. Gross Horsepower
ggplot(data= carsDB, aes(x = hp, y = mpg,col=factor(cyl)))+geom_point() + geom_smooth(color = "Yellow") + labs(title = "Relationship between Fuel Efficiency(Miles/gallon(US)) vs. Gross Horsepower", x = "Gross Horsepower" ,y = "Fuel Efficiency(miles/(US) gallon)",color = "Number of Cylinders")

#Corrolation between mpg and disp
cor(carsDB$mpg,carsDB$hp, use="complete.obs")

#To plot a scatterplot with smoothing for Fuel Efficiency(Miles/gallon(US)) vs. Weight (1000 lbs) )
ggplot(data= carsDB, aes(x = wt, y = mpg,col=factor(cyl)))+geom_point() + geom_smooth(color = "Yellow") + labs(title = "Relationship between Fuel Efficiency(Miles/gallon(US)) vs. Weight (1000 lbs) ", x = "Weight (1000 lbs)" ,y = "Fuel Efficiency(miles/(US) gallon)",color = "Number of Cylinders")

#Corrolation between mpg and disp
cor(carsDB$mpg,carsDB$wt, use="complete.obs")