#!/usr/bin/env Rscript

library(ggplot2)
library(reshape2)
library(pheatmap)
library(scales)

www<-10
hhh<-8

heat_df <- read.csv("~/IdeaProjects/2013-10-01_DisqusThreadsClusters/home/shendrickson/2013-08-07_DisqusTxtDrive/viz/heat_df.csv")

heat <- read.csv("~/IdeaProjects/2013-10-01_DisqusThreadsClusters/home/shendrickson/2013-08-07_DisqusTxtDrive/viz/heat.csv", header=F)

hm <- as.matrix(heat)
hm <- -1.0*hm
####
pdf(file="heat_group.pdf", width=www, height=hhh)
heatmap(hm, col = heat.colors(256))
dev.off()
####
pdf(file="heat_group.2.pdf", width=www, height=hhh)
pheatmap(hm)
dev.off()

heat_df$Cluster <- as.factor(heat_df$Cluster)
X<-melt(heat_df, id=c("Cluster"))

####
X <- within(X, {variable <- reorder(variable, value, FUN=max)})
X <- within(X, {Cluster<- reorder(Cluster, value, FUN=max)})
pdf(file="gg_heat.pdf", width=2*www, height=2*hhh)
ggplot(X) + 
geom_tile(aes(y=Cluster, x=variable, fill=value)) + 
scale_fill_gradient(low="white", high="red") + 
ylab("Topics") + xlab("Threads") 
dev.off()

data <- read.csv("~/IdeaProjects/2013-10-01_DisqusThreadsClusters/home/shendrickson/2013-08-07_DisqusTxtDrive/viz/data.csv")
####
pdf(file="time.pdf", width=www, height=hhh)
ggplot(data) + geom_histogram(aes(time/86400.), binwidth=1, fill="blue")
dev.off()
####
clorder <- within(data, {clusID <-reorder(clusID, time)} )
pdf(file="timebycluster.pdf", width=www, height=hhh)
ggplot(data) + geom_histogram(aes(time/86400.), binwidth=1, fill="red") + 
facet_wrap(~clusID, ncol=5, scale="free_y") +
xlab("Days") + ylab("Comments") + 
theme(strip.text.x = element_text(size=0))
dev.off()
####
thorder <- within(data, {thrID <-reorder(thrID, time)} )
pdf(file="timebythread.pdf", width=www, height=hhh)
ggplot(thorder) + geom_histogram(aes(time/86400.), binwidth=1, fill="red") + 
facet_wrap(~thrID, ncol=5, scale="free_y") + 
xlab("Days") + ylab("Comments") + 
theme(strip.text.x = element_text(size=0))
dev.off()

