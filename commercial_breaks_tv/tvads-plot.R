library(ggplot2)
library(gridExtra)

# creating a vector of time from 8am to 9am
d <- read.csv("fake-tvads-data.csv")
summary(d)
d$t <- strptime(as.character(d$t), '%Y-%m-%d %H:%M:%S')
summary(d)

# Top Plot Data
da <- subset(d, type=="audience")
da <- droplevels(da) # Removes unneeded types or categories
summary(da)

# Bottom Plot Data
d <- subset(d, type %in% c("tune_in", "tune_out"))
d <- droplevels(d)

# http://docs.ggplot2.org/dev/vignettes/themes.html
# http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=4

# cols_pl1 <- c("Percentage of Audience"="black","Beginning of Commercial Break"="red")
# cols_pl <- c("# of STB Tuning In"="blue", "# of STB Tuning Out" = "green")

theme2 <- theme(
  axis.text = element_text(size = 10),
	axis.text.y=element_blank(),
	axis.title=element_blank(),
	axis.ticks.y=element_blank(),
        legend.background = element_rect(fill = "grey90", color="grey", size = 0.07),
        legend.position = c(0.10, 0.80),
        legend.key.height = unit(0.1,'in'),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.key = element_rect(fill = "grey90", colour = "grey90",size = 0.07),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "grey40", size=0.15),
	plot.margin=unit(c(0,1,1,1),"cm")
)

d_copy <- d
d_copy$t <-  as.POSIXct(d_copy$t)

w <- reshape(d_copy[, 2:4], timevar = "type",
             idvar = c("t"), 
             direction = "wide")

w <- w[order((w$value.tune_out - w$value.tune_in), decreasing = TRUE),]

# Crude function for local maxima, take top 7 that are not within a minute of each other

top7_times <-  list(as.POSIXct("1910/1/1"))
top7_times[[1]] <- w[1, 1]

row <- 2
count <- 2


while (count < 8) {
  cur_time <- w[row, 1]
  
  flag <- TRUE
  
  for (idx in 1:length(top7_times)) {
    if (abs(top7_times[[idx]] - cur_time) < as.difftime(1, units = "mins")) {
      if (cur_time < top7_times[[idx]]) {
        top7_times[[idx]] <- cur_time
      }
      flag <- FALSE
      break
    }
  }
  
  if (flag ==TRUE) {
    top7_times[[count]] <- cur_time
    count <-  count + 1
  }
  
  row <-  row + 1
}

top7_times <- top7_times[1:7]


pl <- ggplot(d, aes(x=t, y=value, color=type)) + geom_point(size=0.1) + geom_vline(xintercept = as.numeric(top7_times), colour='grey')
pl <- pl + geom_line(size=0.5)
pl <- pl + scale_color_manual(values=c("blue", "green"), labels = c('# of STB Tuning In', '# of STB Tuning Out'))
pl <- pl + theme2

theme1 <- theme(
  axis.text.y=element_blank(),
  axis.text.x=element_blank(),
	axis.title=element_blank(),
	axis.ticks=element_blank(),
  legend.background = element_rect(fill = "grey90", color="grey"),
  legend.position = c(0.10, 0.80),
  legend.title = element_blank(),
	panel.grid.major.y = element_line(colour = "grey90", size=0.5, linetype="dashed"),
        panel.grid.minor = element_blank(),
	panel.background = element_rect(fill = "white", color = "grey40", size=0.5),
	plot.margin=unit(c(1,1,-0.1,1),"cm"))

pl1 <- ggplot(da, aes(x=t, y=value, color = "black")) + geom_line(size=0.5) + theme1 + labs(x=NULL)

pl1_data <- subset(da, as.numeric(t) %in% unlist(top7_times))


pl1 <- pl1 + geom_segment(data = pl1_data, aes(x=t, y = -Inf, xend = t, yend = value), colour="grey") + 
  scale_color_manual(name = element_blank(),labels = c("Percentage of Audience","Beginning of Commercial Break"), values = c("black","red")) +
  geom_point(data = pl1_data, aes(x = t, y = value, shape = 'Beginning of Commercial Break'), colour="red", fill = "white", show.legend = NA) +
  scale_shape_manual(values=c(21)) +
  scale_fill_manual(values=c('white')) +
  guides(guide_legend())

grid.arrange(pl1, pl, ncol=1, nrow =2, heights = c(0.7, 0.3))



