## import library
library(ggnewscale)
library(ggplot2)
library(dplyr)
library(grid)
library(gtable)
library(gridExtra)

#import raw_data
raw_mat <- read.csv2("DATA/student-mat.csv")
raw_por <- read.csv2("DATA/student-por.csv")
common_student <- merge(raw_mat,raw_por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

cbPalette <- c(  "#A60A55","#4577A0")
palet <- c("#ffd700","#fa8775","#cd34b5","#9d02d7","#CC79A7","#56B4E9")
male_palette <- c("#afcae0","#88afd1","#6095c1","#387ab2")
female_palette <- c("#ce9cb4", "#b66b8f"  , "#9d3969"  , "#850844")


common_student$mean_grade.x <- (common_student$G1.x + common_student$G2.x + common_student$G3.x)/3
common_student$mean_grade.y <- (common_student$G1.y + common_student$G2.y + common_student$G3.y)/3
common_student$studytime_mean <- (common_student$studytime.x + common_student$studytime.y)/2

## Processing data
mat <- common_student[c("sex","G1.x","G2.x", "G3.x", "studytime.x")]
mat$studytime <- mat$studytime.x
mat$studytime.x <- NULL
mat$grad <- mat$G3.x 
mat$G3.x <- NULL
mat$G2 <- mat$G2.x 
mat$G2.x <- NULL
mat$G1 <- mat$G1.x 
mat$G1.x <- NULL
mat$group <- "Mathematics"
por <- common_student[c("sex", "G1.y", "G2.y", "G3.y", "studytime.x")]
por$studytime <- por$studytime.x
por$studytime.x <- NULL
por$grad <- por$G3.y
por$G3.y <- NULL
por$G2 <- por$G2.y
por$G2.y <- NULL
por$G1 <- por$G1.y
por$G1.y <- NULL
por$group <- "Portuguese"
df <- rbind(mat, por)

## scatteR
ggplot() +
  geom_jitter(aes(x=common_student[which(common_student$sex == "M"),]$G3.y, y =common_student[which(common_student$sex == "M"),]$G3.x, color =common_student[which(common_student$sex == "M"),]$studytime_mean),size=2.9, alpha = 1, width = 0.4, height = 0.4) + 
  scale_colour_gradientn(colours = c("#afcae0", "#387ab2"))+
  labs(color="Male and\nstudy hours:")+
  new_scale_color()+
  geom_jitter(aes(x=common_student[which(common_student$sex == "F"),]$G3.y, y =common_student[which(common_student$sex == "F"),]$G3.x, color =common_student[which(common_student$sex == "F"),]$studytime_mean),size=2.9, alpha = 1, width = 0.4, height = 0.4) +
  scale_colour_gradientn(colours = c("#ce9cb4", "#850844"))+
  labs(color="Female and\nstudy hours:")+
  theme_light(base_size = 12)+
  geom_hline(yintercept=mean(common_student$G3.x),  linetype="dashed", color="#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=-0, y=mean(common_student$G3.x)), label="Mean math grade", vjust=1.4, hjust=-0.08, color="#090809", alpha=0.4)+
  geom_vline(xintercept=mean(common_student$G3.y),  linetype="dashed", color= "#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=mean(common_student$G3.y), y=18), label="Mean portuguese grade", hjust=1.1,vjust=-6, color="#090809", angle=0,  alpha=0.4)+
  labs(caption = "Source: UCI Machine Learning Repository - Student Performance Data Set ",
       title="The impact of gender and study hours on student performance in Math and Portoguese classes",
       subtitle = "Examining the correlation between student grade in Math and Portuguese classes with gender represented by point color and\naverage daily study hours epresented by point intensity. It is possible to notice that female are better than male in Portoguese while viceversa in Math.
       It also clear the linear correlation between the two grades: on avarage, the students have the same performance in both the subjects. However, some students have 0 grade in Math while have a higher grade in Portoguese. ")+
  theme(axis.text.x = element_text(size= 12, colour = "grey40"),
        plot.title = element_text(colour="black", face="bold", size=17.45), 
        axis.title.y = element_text(angle = 0, vjust = 0.5, color = "black"), 
        #legend.background = element_rect(fill="white",size=0.5, linetype="solid",colour ="lightgray"),
        legend.box = "vertical",
        legend.position = "right",
        legend.title = element_text(size=12),
        legend.text = element_text(
          color = "grey40", 
          size = 10.8
        ),
        axis.text.y = element_text(size = 12,colour = "grey40"),
        plot.caption = element_text(colour = "grey55"),
        plot.subtitle = element_textbox_simple(
          color = "grey40", 
          size = 10.8,
          lineheight = 1.3, 
          margin = margin(t = 5, b = 30)
        ))+
  ylab("Math grade") +
  scale_y_continuous(limits = c(0, 20), oob = scales::squish)+
  scale_x_continuous(limits = c(0, 20), oob = scales::squish)+
  xlab("Portoguese grade")

ggsave("PLOT/scatter_grade_sex_study_INTENSITY.jpeg", dpi=700, bg="white", width = 13, height = 10)


