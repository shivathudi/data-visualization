library(ggplot2)
library(reshape2)

df <- read.csv('results-gradient-boosting.csv')

cols <- c("Measured = Predicted"="grey","Measured = Predicted - 3%"="grey","Measured = Predicted + 3%"="grey")

p2 <- ggplot(df, aes(x = pred, y = y)) + geom_segment(aes(x = 0, xend = 22, y = 0, yend = 22, color = 'Measured = Predicted')) +
  geom_segment(aes(x = 0, xend = 18, y = 3, yend = 21, color = 'Measured = Predicted - 3%'), linetype = 2) +
  geom_segment(aes(x = 2.5, xend = 22, y = -0.5, yend = 19, color = 'Measured = Predicted + 3%'), linetype = 2) + geom_point(alpha = 0.25) + theme(legend.position=c(.80, .20)) + scale_colour_manual(names(""), values=cols) + xlab("Predicted") +
  ylab("Measured") +
  ggtitle("Measured vs Predicted Passing Rate") + theme(plot.title = element_text(hjust = 0.5))
  
ggsave("plot2.png", scale = 1.1)
