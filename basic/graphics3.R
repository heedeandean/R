trees[c(1:2,30:31),]
H = cut(trees$Height, 4)
H

library(ggplot2)
ggplot(data=trees) +
  geom_point(aes(x=Girth, y=Volume)) +
  geom_point(aes(x=Girth, y=Volume, col=H))

hwy_plot <- ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class))
hwy_plot

hwy_plot +
  geom_smooth(
    mapping=aes(x=displ, y=hwy),
    method='loess' # 비모수적 평활법
  )

hwy_plot +
  geom_smooth(
    mapping=aes(x=displ, y=hwy, linetype=drv),
    method='loess' 
  )

hwy_plot +
  facet_grid(drv ~ cyl)