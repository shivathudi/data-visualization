library(ggplot2)

## PDFS


x <- seq(0,1,length=100)

beta1 <- dbeta(x, 0.5, 0.5)
beta2 <- dbeta(x, 5, 1)
beta3 <- dbeta(x, 1, 3)
beta4 <- dbeta(x, 2, 2)
beta5 <- dbeta(x, 2, 5)

df <- data.frame(x,beta1, beta2, beta3, beta4, beta5)

df <- gather(df, func, val, -x)

greeks <- list(bquote(paste("{", alpha, ",", beta, "} = ", .(paste("{",0.5,",",0.5, "}", sep = "")), collapse = "")),bquote(paste("{", alpha, ",", beta, "} = ", .(paste("{",5,",",1, "}", sep = "")), collapse = "")),bquote(paste("{", alpha, ",", beta, "} = ", .(paste("{",1,",",3, "}", sep = "")), collapse = "")),bquote(paste("{", alpha, ",", beta, "} = ", .(paste("{",2,",",2, "}", sep = "")), collapse = "")),bquote(paste("{", alpha, ",", beta, "} = ", .(paste("{",2,",",5, "}", sep = "")), collapse = "")))

gg <- ggplot(df, aes(x=x, y=val, group=func)) + 
  geom_line(aes(color=func)) + 
  scale_y_continuous(expand=c(0, 0)) + 
  scale_color_manual(name="Beta params", 
                     values=c("red", "green", "blue", "violet", "black"),
                     labels=greeks)+ 
  labs(x="\nx value", y="PDF\n",
       title="Beta Probability Distribution Functions\n")+
  theme(strip.text = element_text(size = 18, angle = 90)) + 
  theme(axis.text=element_text(size=15),axis.title=element_text(size=20)) + 
  theme(plot.title = element_text(size=22))

gg

ggsave("Beta Probability Distribution Functions.png")

### CDFS

x <- seq(0,1,length=100)

beta1 <- pbeta(x, 0.5, 0.5)
beta2 <- pbeta(x, 5, 1)
beta3 <- pbeta(x, 1, 3)
beta4 <- pbeta(x, 2, 2)
beta5 <- pbeta(x, 2, 5)

df <- data.frame(x,beta1, beta2, beta3, beta4, beta5)

df <- gather(df, func, val, -x)

gg <- ggplot(df, aes(x=x, y=val, group=func))+ 
  geom_line(aes(color=func))+scale_y_continuous(expand=c(0, 0))+
  scale_color_manual(name="Beta params",
                     values=c("red", "green", "blue", "violet", "black"),
                     labels=greeks)+
  labs(x="\nx value", y="CDF\n",
       title="Beta Cumulative Distribution Functions\n")+
  theme(strip.text = element_text(size = 18, angle = 90)) + 
  theme(axis.text=element_text(size=15),axis.title=element_text(size=20)) + 
  theme(plot.title = element_text(size=22))

gg

ggsave("Beta Cumulative Distribution Functions.png")

### FACETED PDFS AND CDFS

x_vals <- seq(0,1,length=100)

#These tuple values can be replaced and the graphs will update accordingly

my_tuple_values <- list(c(0.5,0.5), c(5,1), c(1,3), c(2,2), c(2,5))

mydf <- data.frame()

df_list <- lapply(my_tuple_values, function(x, df=mydf) {
  alpha_param =x[[1]]
  beta_param=x[[2]]
  y_val <- dbeta(x_vals,alpha_param,beta_param)
  type <- rep("pdf", length(y_val))
  
  tuple <- rep(paste("{",alpha_param,",",beta_param,"}", sep = ""),length(y_val))
  
  new_df <- data.frame(x_vals,y_val, type, tuple)
  new_df
})



mypdf <- do.call(rbind,df_list)

df_list_2 <- lapply(my_tuple_values, function(x, df=mydf) {
  alpha_param =x[[1]]
  beta_param=x[[2]]
  y_val <- pbeta(x_vals,alpha_param,beta_param)
  type <- rep("cdf", length(y_val))
  tuple <- rep(paste("{",alpha_param,",",beta_param,"}", sep = ""),length(y_val))
  new_df <- data.frame(x_vals,y_val, type, tuple)
  new_df
})

mycdf <- do.call(rbind,df_list_2)

mydf <- rbind(mypdf, mycdf)

mydf$tuple <- as.character(mydf$tuple)

my_plot <- ggplot(mydf, aes(x=x_vals, y=y_val, color = tuple)) + 
  geom_line() + 
  facet_grid(tuple ~ type, scales = "free", labeller = label_bquote(paste("{", alpha, ",", beta, "} = ", .(tuple), collapse = ""))) + 
  theme(strip.text = element_text(size = 13)) + 
  theme(axis.text=element_text(size=15),axis.title=element_text(size=20)) + 
  theme(plot.title = element_text(size=22)) +
  labs(x="\nx value", y="Distribution Function Value\n",title="Beta Distribution Functions\n")

my_plot

ggsave("Beta Distribution Functions.png")


