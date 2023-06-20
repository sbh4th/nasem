library(readxl)
library(tidyverse)
library(here)
library(ggrepel)
library(patchwork)
library(directlabels)

d <- read_excel(here("data", "hinone.xlsx"), 
                     range = cell_rows(1:12)) %>%
  rename(Group = `Characteristic`)

d2 <- d %>% pivot_longer(!`Group`, 
  names_to = "Year", values_to = "Pct") %>%
  mutate(Year = as.numeric(Year))

# theme properties
stheme <- theme_classic() + theme(plot.title = element_text(size = 16, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="grey60"), axis.line.y=element_line(colour="grey60"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

d3 <- d2 %>% filter(Group=="NH White" | Group=="NH Black") 

p1 <- d3 %>%
  ggplot(aes(x=Year, y=Pct, colour=Group, label=Group)) +
  geom_line(aes(colour=Group), size=1.5, show.legend=FALSE) +
  geom_text_repel(data=subset(d3, Year==2019), xlim  = 2023, 
                  hjust = 1, point.padding = 0.5) +
  scale_y_continuous(limits=c(0,50), breaks=c(0,10,20,30,40)) +
  scale_x_continuous(limits=c(1999,2027), 
                     breaks=c(2000,2005,2010,2015,2020)) + 
  ylab("") + xlab("") + scale_color_brewer(palette = "Dark2") +
  ggtitle("% under 65 without health insurance 1999-2019") + stheme + theme(legend.position = "none", 
        panel.grid.major.y = element_line(colour="white"))

# export to file
ggsave(here("images", "hi-trends-bw.png"), 
       plot=p1, width=10, height=5.625)


d4 <- d2 %>% filter(Group!="Hispanic" & 
                      Group!="Not Hispanic") 

p2 <- d4 %>%
  ggplot(aes(x=Year, y=Pct, colour=Group, label=Group)) +
  geom_line(aes(colour=Group), size=1.5, show.legend=FALSE) +
  geom_text_repel(data=subset(d4, Year==2019), xlim  = 2023, 
                  hjust = 1, point.padding = 0.5) +
  scale_y_continuous(limits=c(0,50), breaks=c(0,10,20,30,40)) +
  scale_x_continuous(limits=c(1999,2027), 
                     breaks=c(2000,2005,2010,2015,2020)) + 
  ylab("") + xlab("") + scale_color_brewer(palette = "Dark2") +
  ggtitle("% under 65 without health insurance 1999-2019") + stheme + theme(legend.position = "none", 
        panel.grid.major.y = element_line(colour="white"))

# export to file
ggsave(here("images", "hi-trends-all.png"), 
       plot=p2, width=10, height=5.625)
