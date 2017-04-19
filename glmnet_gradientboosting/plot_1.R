library(ggplot2)
library(reshape2)

df_glmnet <- read.csv('results-glmnet-cross-table.csv')
df_glmnet['group'] <- "glmnet"

df_boosting <- read.csv('results-pgboost-cross-table.csv')
df_boosting['group'] <-  "boosting"

df_merged <- rbind(df_boosting, df_glmnet)
names(df_merged) <- c("iter", "RMSE", "MedErr", "group")

df_final <- melt(df_merged[, -1], id.vars = "group")

theme1 <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_text(size = rel(1.5)),
  axis.text.y = element_text(size=rel(1.3)),
  strip.text.x = element_text(size = rel(1.5)),
  plot.title = element_text(size = rel(1.5), hjust = 0.5)
)

p2 <- ggplot(df_final, aes(x=group, y = value)) + 
  geom_boxplot(outlier.size = 2.5) + facet_wrap(~factor(df_final$variable, levels = c("MedErr", "RMSE")), scales = "free_y") +ggtitle("Comparing glmnet and gradient boosting") + theme1 

ggsave("plot1.png", scale = 1.1)
