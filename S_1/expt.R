# https://learnr.wordpress.com/2009/03/16/ggplot2-plotting-two-or-more-overlapping-density-plots-on-the-same-graph/

library(ggplot2)

df_f = data.frame(x = rnorm(1000,0,1), y=rnorm(1000,0,2),z=rnorm(1000,2,1.5))
df_f

library(reshape2)

df_m = melt(df_f)
df_m

ggplot(df_m)+geom_freqpoly(aes(x=value, y=..density.., colour=variable))

ggplot(df_m)+geom_density(aes(x=value, colour=variable))

















