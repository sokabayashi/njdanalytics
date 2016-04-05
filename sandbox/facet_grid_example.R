library("ggplot2")
library("dplyr")
library("reshape2")

head( mtcars )

df <- mtcars %>%
  add_rownames(var = "Model") %>% # car model is a rowname.  make into its own column in df
  select(Model, wt, mpg, carb)

head( df )

df_m <- df %>% melt(id.vars = c("Model","wt"))

ggplot(df_m)+
  aes(x=wt, y=value)+
  geom_point()+
  theme(strip.background=element_blank())+
  facet_grid(variable ~ ., scales="free_y", switch="y")
