library(arules)
library(bnlearn)
library(Rgraphviz)
library(igraph)

# Read in the csv file (assuming it is in the current R working directory)
maintain<-read.csv(file="maintenance_data.csv")



discrete_maintenance <- NULL
discrete_maintenance$pressureInd<-arules::discretize(maintain$pressureInd,method="interval",categories=2,labels=c("low","high"))
discrete_maintenance$pressureInd
discrete_maintenance$moistureInd<-arules::discretize(maintain$moistureInd,method="interval",categories=2,labels=c("low","high"))
discrete_maintenance$moistureInd
discrete_maintenance$temperatureInd<-arules::discretize(maintain$temperatureInd,method="interval",categories=2,labels=c("low","high"))
discrete_maintenance$temperatureInd
discrete_maintenance$team <- maintain$team
discrete_maintenance$team
discrete_maintenance$provider <- maintain$provider
discrete_maintenance$provider
discrete_maintenance$broken <- maintain$broken
discrete_maintenance$broken
discrete_maintenance<-as.data.frame(discrete_maintenance)
discrete_maintenance$lifetime<-arules::discretize(maintain$lifetime,method="frequency",categories=3,labels=c("short","medium","high"))

discrete_maintenance

bn<-gs(discrete_maintenance)
fittedbn<-bn.fit(bn,discrete_maintenance)

par(list(nodes = list( textCol = "red",fontsize=50)))
gridGraphviz.plot(bn,layout="fdp",main="SPI Bayesian Network")
write.csv(file="bayesMB.csv",discrete_maintenance)

dag.igraph<-igraph.from.graphNEL(as.graphNEL(fittedbn))
plot(dag.igraph,vertex.color="yellow",vertex.size=20)
tkplot(dag.igraph,vertex.color="yellow",vertex.size=20)

cpquery(fittedbn,evidence=(provider=="Provider4" & lifetime=='shortest'),event=(broken=="No"),n=1000000)