library( njdanalytics )
# http://stackoverflow.com/questions/24234791/plot-multiple-ggplot-plots-on-a-single-image-with-left-alignment-of-the-plots-an

# Bind plots above one another so that axes align





df <- mtcars %>%
  add_rownames(var = "Model") %>% # car model is a rowname.  make into its own column in df
  select(Model, wt, mpg, carb)

# http://stackoverflow.com/questions/15556068/removing-all-the-space-between-two-ggplots-combined-with-grid-arrange?rq=1
# plot.margin: set negative value for the bottom margin for top chart and upper margin for bottom chart. This will ensure that both plot joins.
# margin: top __ bottom __

theme_top    <- theme(legend.position="none", plot.margin=unit(c(1,1,-0.5,1), "cm"),
                      axis.text.x=element_blank(), axis.ticks.x=element_blank() )
theme_middle <- theme(legend.position="none", plot.margin=unit(c(-0.3,1,-0.3,1), "cm"),
  axis.text.x=element_blank(), axis.ticks.x=element_blank() )

theme_bottom <- theme(legend.position="none", plot.margin=unit(c(-0.5,1,1,1), "cm"))

p.wt   <- ggplot(df,aes(x=Model,y=wt)) + geom_bar(stat="identity") + xlab(NULL) + theme_bw() + theme_top
p.mpg  <- ggplot(df,aes(x=Model,y=mpg)) + geom_bar(stat="identity")+ xlab(NULL) + theme_few() + theme_middle
p.carb <- ggplot(df,aes(x=Model,y=carb)) + geom_bar(stat="identity") + theme_bw() + theme_bottom


# TO DO
# 1. For all but bottom chart
#   x axis label
#   Remove tick marks
#



my_plots <- list(p.wt, p.mpg, p.carb)
gp <- do.call(rbind_max, my_plots )
# gp <- gtable_add_cols(gp, widths = sum(leg$widths))
panels <- gp$layout$t[grep("panel", gp$layout$name)]
# set the relative panel heights
gp$heights[panels] <- lapply(c(5,3,1), unit, "null")
# set the legend justification to top (it's a gtable embedded in a gtable)
# leg[["grobs"]][[1]][["vp"]] <- viewport(just = c(0.5,1))
# gp <- gtable_add_grob(gp, leg, t = 1, l = ncol(gp))

grid.newpage()
grid.draw(gp)
