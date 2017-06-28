library(ggplot2)
pl<- ggplot(maintenance,aes(x=lifetime))
pl + geom_histogram()
print (pl + geom_histogram(binwidth = 10))

