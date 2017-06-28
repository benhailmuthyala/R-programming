library(ggplot2)
pl<- ggplot(maintenance,aes(x=provider,y=lifetime))
 pl + geom_point()
print (pl + geom_point(size=5))

