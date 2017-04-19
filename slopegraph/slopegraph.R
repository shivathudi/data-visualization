library(ggplot2)
library(reshape2)

df <- read.csv('cancer_survival_rate.csv')
df <- df[, c(1, 2, 3, 5)]

# df_2 <- reshape(df, varying = c("X5.year", "X10.year", "X20.year"), direction = "long", idvar = "Cancer.type", v.names = "Value", timevar = "Year", times = c("5", "10", "20"))

p<-ggplot(df) + geom_segment(aes(x=0,xend=100,y=X5.year,yend=X10.year),size=.75) + geom_segment(aes(x=100,xend=200,y=X10.year,yend=X20.year),size=.75)

theme1 <- theme(axis.line = element_blank(),
      axis.text = element_text(colour="black"),
      axis.text.x = element_text(size = rel(1), lineheight = 0.9,
                                 vjust = 1),
      axis.text.y = element_text(size=rel(0.8)),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      axis.ticks.margin = unit(0, "lines"), 
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.margin = unit(0.25, "lines"), 
      strip.background = element_blank(),
      strip.text.x = element_text(size = rel(0.8)),
      strip.text.y = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(size = rel(1)),
      plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines"),
      complete=FALSE)


p<-p + theme(panel.background = element_blank())
p<-p + theme(panel.grid=element_blank())
p<-p + theme(axis.ticks=element_blank())
p<-p + theme(axis.text=element_blank())
p<-p + theme(panel.border=element_blank())

p + theme1

tmp <- df

ord <- order(tmp[,2])
tmp <- tmp[ord,]
min.space <- 0.05

min.space <- min.space*diff(range(tmp[,-1]))
yshift <- numeric(nrow(tmp))
## Start at "bottom" row
## Repeat for rest of the rows until you hit the top
for (i in 2:nrow(tmp)) {
  ## Shift subsequent row up by equal space so gap between
  ## two entries is >= minimum
  mat <- as.matrix(tmp[(i-1):i, -1])
  d.min <- min(diff(mat))
  yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
}


tmp <- cbind(tmp, yshift=cumsum(yshift))

scale <- 1
tmp <- melt(tmp, id=c("Cancer.type", "yshift"), variable.name="x", value.name="y")
## Store these gaps in a separate variable so that they can be scaled ypos = a*yshift + y

tmp <- transform(tmp, ypos=y + scale*yshift)
return(tmp)

df <- tmp

ylabs <- subset(df, x==head(x,1))$Cancer.type
yvals <- subset(df, x==head(x,1))$ypos
fontSize <- 2.5
gg <- ggplot(df,aes(x=x,y=ypos)) +
  geom_line(aes(group=Cancer.type),colour="grey80") +
  geom_point(colour="white",size=8) +
  geom_text(aes(label=y),size=fontSize) +
  scale_y_continuous(name="", breaks=yvals, labels=ylabs)
gg.form <- gg + theme1
gg.form
