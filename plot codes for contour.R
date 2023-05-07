require(ggplot2)
require(ggpubr)
require(tidyverse)
require(ContourFunctions)
require(laGP)
require(lattice)
require(metR)
#These packages are required for prepare the data to make the contour plot. 

x<- seq(15,115, length = 500) #Required Temperature range
x
y <- seq(-9.94, -6.5, length = 500) #Required d18O(VPDB) composition
#Creating the matrix of plot
df=expand.grid(x=x,y=y)
df$z= (((df$y*1.03092+30.92)+1000)/exp((((2.9923*10^6)/((df$x+273.15)^2))-2.3592)/1000))-1000 # Constructed for dolomite using the equation of Müller et al. (2019) 
df$z # Calculated d18O of water for matrix
format(round(df$z, 2), nsmall = 2)

#This plots the backgroud with contour lines and points can be added as another layer on top of the background.
ggplot() +
  geom_tile(data=df, aes(x=df$x,y=df$y,z=df$z,fill=df$z))+
    stat_contour(data=df, aes(x=df$x,y=df$y,z=df$z,fill=df$z),color = 'Black', linetype = 1, size =0.7)+
  scale_fill_gradientn(colours=colorRampPalette (c ("Cyan3", "Gold")) (5))+
  metR::geom_text_contour(data=df, aes(x=x,y=y,z=z,label = round(..level.., 2)), size = 6, label.placer = label_placement_n())+
    labs(x = "Temperature(°C)" , fill= expression(paste(delta^18,"O"[VSMOW],"(\u2030)water")),color = "Dolomite specimen", shape = 'Dolomite specimen')+
    ylab(bquote(delta ^ . (18)~O[VPDB]~"(‰)"))+
    guides(color = guide_legend(order = 1))+
  theme_test()+
  theme(panel.border = element_rect(color = "Black", size = 1.5))+
 theme(axis.title = element_text(size = 12, colour = "Black", face = "bold"))+ 
  theme(axis.text = element_text(size = 12, colour = "Black",face = "bold"))+
  theme(legend.position = c(0.18,0.845), legend.box = "horizontal")+
   theme(legend.box.background = element_rect(colour = "Black", size = 0.5), legend.box.margin = margin(6,6,6,6))


