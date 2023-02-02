library(ggplot2)
library(ggtext)
library(ragg)

#### Dataset preprocessing ####
setwd("C:/Users/andre/OneDrive/Documenti/UniTs/Secondo Anno/IR and DV/Data Visualization/Project/student")

d1=read.table("DATA/student-mat.csv",sep=";",header=TRUE)
d2=read.table("DATA/student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

avg.math = mean(d3$G3.x)
sd.math = sd(d3$G3.x)
avg.port = mean(d3$G3.y)
sd.port = sd(d3$G3.y)

low = d3[d3$G3.x<avg.math,]
low = low[low$G3.y<avg.port,]
high = d3[d3$G3.x>avg.math,]
high = high[high$G3.y>avg.port,]

hi <- rbind(mean(high$Walc.x+high$Dalc.x)/2,mean(high$freetime.x+high$freetime.y)/2,mean(high$Medu+high$Fedu)/2,mean(high$health.x),mean(high$famrel.x),mean(high$goout.x))
lo <- rbind(mean(low$Walc.x+low$Dalc.x)/2,mean(low$freetime.x+low$freetime.y)/2,mean(low$Medu+low$Fedu)/2,mean(low$health.x),mean(low$famrel.x), mean(low$goout.x))
avg <- rbind(mean(d3$Walc.x+d3$Dalc.x)/2,mean(d3$freetime.x+d3$freetime.y)/2,mean(d3$Medu+d3$Fedu)/2,mean(d3$health.x),mean(d3$famrel.x), mean(d3$goout.x))

names <- c("Alcohol consumption","Free time after school","Parental Education","Health status","Family relations", "Break out with friends")
diff <- abs(hi-lo)

rad <- data.frame(names = as.factor(names),hi = hi,lo=lo, diff = diff, global = avg)

rad$names = with(rad, reorder(names, global, mean))


expl = "The following graph compares two groups of students: the ones with a mean G3 grade (between Portuguese and Math) above the average and the ones with mean G3 grade 
        below it. Respectively, the groups have 139 and 127 students. Each row represents a question whose response ranges from 1 to 5, where the higher the number the better/greater the feature. For each question the average response of the group
        is reported."
comm = "On average, \"good\" students have parents with higher education and devote more time to study. Bad ones instead, drink more alcohol and hang out with friends more and, 
        interestingly, have a better health status."

#### Graph ####

theme_set(theme_minimal(base_size = 12))
theme_update( 
  plot.margin = margin(25, 15, 15, 25),
  plot.background = element_rect(color = "#FFFCFC", fill = "#FFFCFC"),
  panel.grid.major.x = element_line(color = "grey94"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(color = "grey40", size = 15),
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  #axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
  #legend.position = c(.95,0.2), 
  legend.position = "top",
  axis.ticks = element_blank(),
  legend.title = element_text(
    hjust = .5,
    size = 12
  ),
  legend.text = element_text(
    color = "grey40", 
    size = 12
  ),
  legend.box = "horizontal",
  legend.box.just = "bottom",
  legend.margin = margin(0, 0, 0, 0),
  legend.spacing = unit(.6, "lines"),
  plot.title = element_text(
    face = "bold", 
    size = 17.45,
    hjust = 0
  ),
  plot.subtitle = element_textbox_simple(
    color = "grey40", 
    size = 10.8,
    lineheight = 1.3, 
    margin = margin(t = 5, b = 30)
  ),
  plot.caption = element_text(
    color = "grey55", 
    size = 10.5, 
    margin = margin(t = 20, b = 0, r = 15)
  )
)

cus_green <- rgb(0.2,0.7,0.1,0.9)
cus_red <- rgb(0.7,0.2,0.1,0.9)
cus_green <- '#58a07e'
cus_red <- '#c54d71'

ggplot(rad) +
  #geom_segment( aes(x = names, xend= names,y=.9, yend=global), linetype = "solid", alpha = .2, size = 10)+
  geom_linerange( aes(x=names, xmax = names, ymin=lo, ymax=hi, color = "grey"), size = 1, alpha = .5)+
  geom_point( aes(x=names, y=hi, color = cus_green), size=8) +
  geom_point( aes(x=names, y=lo, color = cus_red), size=8 ) +
  geom_point( aes(x=names, y=global, color = "grey"), size=5) +
  coord_flip()+
  geom_text(aes(x=names, y = .9, label = names), hjust = 1, nudge_x = 0.025, size = 4)+
  
  #geom_text(aes(x=names, y = lo , label = round(lo,1)), hjust = .5, nudge_x = 0, size = 3)+
  #geom_text(aes(x=names, y = hi , label = round(hi,1)), hjust = .5, nudge_x = 0, size = 3)+
  scale_y_continuous(limits = c(.5,4), breaks= seq(1,4,.5), labels = c(1,"",2,"",3,"",4))+
  xlab("") +
  ylab("Student's vote")+
  scale_color_identity(name = "Group:",
                       breaks = c(cus_green, cus_red, "grey"),
                       labels = c("Above average", "Below average", "Global"),
                       guide = "legend")+
  labs(
    title = "Do \"good\" students differ from \"bad\" ones?",
    subtitle = paste(expl,comm),
    caption = "Source: UCI Machine Learning Repository - Student Performance Data Set"
  )


ggsave("goodbad_plot.png", device = "png", width = 13, height = 8, limitsize = FALSE, dpi = 600)

