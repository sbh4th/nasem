#  program:  rural-upoison-trends.R
#  task:     urban-rural poisoning trends
#  input:    unint-poison-ur-1999-2018-allages.txt
#  output:   rural-upoison-trends.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-25

##### 0 #####
##### load libraries
library(tidyverse)
library(here)
library(ggrepel)
library(patchwork)
library(directlabels)

here::here()

##### 1  #####
##### Read in data

upur <- read_tsv(here("data", 
                      "unint-poison-ur-1999-2020-allages.txt"), skip=1, 
  col_names=c("notes", "ur", "urcode", "year", "ycode", 
              "deaths", "pop", "crate", "arate"), n_max=132,
  col_types = "ccddddddd")


##### 2  #####
##### Make the plot

# theme properties
stheme <- theme_classic() + theme(plot.title = element_text(size = 16, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))



## plot it
p <- ggplot(upur, aes(x=year, y=arate, colour=ur, label=ur)) +
  geom_segment(aes(x = 1999, y = 10, xend = 2020, yend = 10), 
               linetype="dotted", color='grey60', show.legend=FALSE) +
  geom_segment(aes(x = 1999, y = 20, xend = 2020, yend = 20), 
               linetype="dotted", color='grey60', show.legend=FALSE) +
  geom_line(aes(colour=ur), size=1.5, show.legend=FALSE) + 
  geom_text_repel(data=subset(upur, year==2020), xlim  = 2023, 
                  hjust = 1, point.padding = 0.5) +
  geom_segment(aes(x = 2000, y = 15, xend = 2010, yend = 15), 
               color='grey60', show.legend=FALSE) + 
  geom_segment(aes(x = 2010, y = 20.5, xend = 2015, yend = 20.5), 
               color='grey60', show.legend=FALSE) + 
  geom_segment(aes(x = 2013, y = 31, xend = 2019, yend = 31), 
               arrow = arrow(length = unit(0.25, "cm")), color='grey60', 
               show.legend=FALSE) + 
  scale_y_continuous(limits=c(0,32), breaks=c(0,10,20,30)) +
  scale_x_continuous(limits=c(1998,2027), 
                     breaks=c(2000,2005,2010,2015,2020)) + 
  ylab("") + xlab("") + 
  ggtitle("Age-adjusted unintentional poisoning death rate per 100,000, 1999-2020") + stheme + 
  theme(legend.position = "none", 
        panel.grid.major.y = element_line(colour="white")) +
  scale_color_manual(values=c('#08519c','#3182bd','#6baed6','#de2d26','#a50f15','#9ecae1','#bdbdbd')) + scale_fill_manual(values=c('#08519c','#3182bd','#6baed6','#de2d26','#a50f15','#9ecae1','#bdbdbd')) + 
  annotate(geom="text", x=2005, y=15.7, label="Prescription painkiller phase", 
           color='grey60', cex=5) + 
  annotate(geom="text", x=2012.5, y=21.5, label="Heroin phase", 
           color='grey60', cex=5) + 
  annotate(geom="text", x=2016, y=32, label="Synthetic opioid phase", 
           color='grey60', cex=5)

# export to file
ggsave(here("images", "rural-upoison-trends.png"), 
       plot=p, width=10, height=5.625)
