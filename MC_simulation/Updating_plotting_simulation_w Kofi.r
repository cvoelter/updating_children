	
setwd("C:/Users/cjv3/R/R data analysis/EF battery/Updating_exp1")


getwd()

xdata=read.table(file="C:/Users/cjv3/R/R data analysis/EF battery/Updating_exp1/Updating_exp1_plotting data simulation_w Kofi.txt", header=T, sep="\t")


str(xdata)
summary(xdata)


#install.packages("ggplot2")






library(grid)
library(ggplot2)


		dodge <- position_dodge(width = 0.9)

		g1 <- ggplot(data = xdata, aes(x = simulation, y = error, fill = position)) +  labs(x = NULL, y = "Probability to committ an error",
				 fill = "Error position")+
		  geom_bar(stat = "identity", position = position_dodge()) +scale_y_continuous(expand = c(0,0)) +
		  geom_errorbar(aes(ymax = error + se, ymin = error - se), position = dodge, width = 0.2) +
		  coord_cartesian(ylim = c(0, 1)) +
		 # geom_vline(xintercept = 2.5, colour="white")+
		 geom_segment(aes(x = 6.5, y = -0.13,  xend=6.5, yend = 0), colour = "black", size=0.5)+
		# geom_hline(yintercept = 0.5, linetype="dashed")+


	annotate("text", x = rep(c(1:6)), y = - 0.03,
		     label = c(0:5)) +

	# annotate("text", c(3.5,7.2,8.3), y = - 0.07, label = c("Memory size", "Feature + Space", "Feature only"), fontface =1) +
#	  annotate("text", c(3.5), y = - 0.06, label = c("Memory size"), fontface =1) +
	 
	 annotate("text", c(7.2,8.3), y = - 0.03, label = c("Feature + Space", "Feature only"), fontface =1) +			   
			   
	  annotate("text", c(3.5, 7.75), y = - 0.1, label = c("Simulation of different memory sizes", "Chimpanzees"), fontface =2) +
			   
		 # Use grey scale
		scale_fill_grey(start=0.9, end=0.1) + theme_classic()+
		  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
			   axis.title.x = element_blank(),
			   axis.text.x = element_blank())
					   
			   
			   
				g1

			
		# remove clipping of x axis labels
		g2 <- ggplot_gtable(ggplot_build(g1))
		g2$layout$clip[g2$layout$name == "panel"] <- "off"
		
			
			g3 <- grid.draw(g2)

		
		
		
#ggsave(filename="myPlot.pdf")

	tiff(filename = "Updating_simulation.tif", compression="lzw", res=1200)
	grid.draw(g2)
	dev.off()

tiff("Updating_exp1_simulation_con_new2.tif", width = 27, height = 18, units = 'cm', res = 1200, compression = 'lzw')

grid.draw(g2)

dev.off()

tiff("Updating_exp1_simulation_lower res_con_new.tif", width = 27, height = 18, units = 'cm', res = 300, compression = 'lzw')

grid.draw(g2)

dev.off()




