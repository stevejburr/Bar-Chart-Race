library(tidyverse)
library(gganimate)
#This is the stepwise code I developed to produce a function to make bar chart race charts
#Goal: build a function to do a gganimate bar chart race.

data <- read_csv('https://gist.githubusercontent.com/johnburnmurdoch/2e5712cce1e2a9407bf081a952b85bac/raw/08cf82f5e03c619f7da2700d1777da0b5247df18/INTERBRAND_brand_values_2000_2018_decimalised.csv')

#View(head(data))

#set up basic plot with a single time stamp
data %>%
  filter(year==2018.0) %>%
  filter(rank<=10) %>% 
  mutate(name=as.factor(name),
         name=fct_reorder(name,-rank)) %>%
  ggplot(aes(x=name,y=value,fill=name)) +
  geom_col(show.legend = F)+
  geom_text(aes(label=name),
            hjust="right",
            colour="black",
            fontface="bold",
            nudge_y=-1000)+
  geom_text(aes(label=scales::comma(value)),
            hjust="left",
            nudge_y=2000,
            colour="grey30")+
  theme_minimal() +
  coord_flip() +
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma)+
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        axis.text.y=element_blank())

#to use gganimate, need to use geom_tile instead
data %>%
  filter(year==2018.0) %>%
  filter(rank<=10) %>% 
  mutate(name=as.factor(name),
         name=fct_reorder(name,-rank)) %>%
  ggplot(aes(x=name,y=value,fill=name)) +
  geom_tile(aes(y=value/2,height=value),width=0.9,show.legend = F)+
  geom_text(aes(label=name),
            hjust="right",
            colour="black",
            fontface="bold",
            nudge_y=-1000)+
  geom_text(aes(label=scales::comma(value)),
            hjust="left",
            nudge_y=2000,
            colour="grey30")+
  theme_minimal() +
  coord_flip() +
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma)+
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        axis.text.y=element_blank())


#extract / identify a colour for each distinct value of the label
#the position on each plot is unique per frame


data %>%
  filter(rank<=10) %>% 
  ggplot(aes(x=-rank,y=value,fill=name, group=name)) +
  geom_tile(aes(y=value/2,height=value),width=0.9,show.legend = F)+
  geom_text(aes(label=name),
            hjust="right",
            colour="black",
            fontface="bold",
            nudge_y=-1000)+
  geom_text(aes(label=scales::comma(value)),
            hjust="left",
            nudge_y=2000,
            colour="grey30")+
  theme_minimal() +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma)+
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        axis.text.y=element_blank()) +
  transition_time(year)-> p


animate(p, nframes = 300, fps = 5, end_pause = 20)


#inputs:
#data - the dataset, must contain a column called "year"
#x - the column which contains the numeric value to plot 
#y - the column which contains the labels of the plot

#optional: 
#title - title to show at the top of the graph, defaults to blank
#caption - text show in footer, defaults to blank
#number - filter to top "number" defaults to 10 for top 10 in each period
#parameters passed to "animate" function: nframes=300, fps=5, end_pause=20

make_barchart_race <- function(data,x,y,
                               number=10,
                               title="",
                               caption="",
                               nframes=300,
                               fps=5,
                               end_pause=20){
  #set up variables for use with tidy evaluation
  y <- rlang::enquo(y)
  x <- rlang::enquo(x)
  number <- rlang::enquo(number)
  
  #take the input dataset, compute ranks within each time period
  data %>%
    group_by(year) %>%
    arrange(-!!y) %>%
    mutate(rank=row_number()) %>%
    #filter to top "number"
    filter(rank<=!!number) -> data
  
  #plot the data
  data %>%
    ggplot(aes(x=-rank,y=!!y,fill=!!x, group=!!x)) +
    geom_tile(aes(y=!!y/2,height=!!y),width=0.9,show.legend = F)+
    geom_text(aes(label=!!x),
              hjust="right",
              colour="black",
              fontface="bold",
              nudge_y=-1000)+
    geom_text(aes(label=scales::comma(!!y)),
              hjust="left",
              nudge_y=2000,
              colour="grey30")+
    theme_minimal() +
    coord_flip(clip="off") +
    scale_x_discrete("") +
    scale_y_continuous("",labels=scales::comma)+
    theme(panel.grid.major.y=element_blank(),
          panel.grid.minor.x=element_blank(),
          plot.subtitle = element_text(size=20,colour="grey50",face="bold"),
          plot.margin = margin(1,1,1,2,"cm"),
          axis.text.y=element_blank())+
    #this bit does the animation by year
    transition_time(year) +
    labs(title=title,
         subtitle='{round(frame_time,0)}',
         caption=caption)-> p
  
  #animate the plot - this is returned by the function
  animate(p, nframes = nframes, fps = fps, end_pause = end_pause)
}
test <-data
make_barchart_race(test,name,value)
anim_save("out.gif")
