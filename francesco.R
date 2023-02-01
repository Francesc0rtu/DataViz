## import library
library(ggplot2)
library(dplyr)
library(grid)
library(gtable)
library(gridExtra)

#import raw_data
raw_mat <- read.csv2("DATA/student-mat.csv")
raw_por <- read.csv2("DATA/student-por.csv")
common_student <- merge(raw_mat,raw_por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

## Basic-Analysis
summary(raw_mat)

## Boxplot for grade and classes
plot_comparison <- function(category){
  M_n1 <- ggplot(data = raw_mat) + geom_boxplot(aes( x=as.factor(get(category)), y=G1, fill=as.factor(get(category))))
  M_n2 <- ggplot(data = raw_mat) + geom_boxplot(aes(x=as.factor(get(category)), y=G2, fill=as.factor(get(category))))
  M_n3 <- ggplot(data = raw_mat) + geom_boxplot(aes(x=as.factor(get(category)), y=G3, fill=as.factor(get(category))))
  
  P_n1 <- ggplot(data = raw_por) + geom_boxplot(aes(x=as.factor(get(category)), y=G1, fill=as.factor(get(category))))
  P_n2 <- ggplot(data = raw_por) + geom_boxplot(aes(x=as.factor(get(category)), y=G2, fill=as.factor(get(category))))
  P_n3 <- ggplot(data = raw_por) + geom_boxplot(aes(x=as.factor(get(category)), y=G3, fill=as.factor(get(category) ) ) ) 
  
  
  ## grid plot
  grid.arrange(
    arrangeGrob(M_n1, M_n2, M_n3, ncol = 3, top="Math"),
    arrangeGrob(P_n1, P_n2, P_n3, ncol = 3, top="Port"),
    nrow = 2,
    top = category
  )
}
## Produce plots
plot_comparison("nursery")
plot_comparison("romantic")
plot_comparison("internet")
plot_comparison("sex")
plot_comparison("famsize")
plot_comparison("freetime")
plot_comparison("activities")
plot_comparison("famsup")
plot_comparison("Fedu")
plot_comparison("Medu")
plot_comparison("age")
plot_comparison("Dalc")
plot_comparison("Walc")
plot_comparison("studytime")
plot_comparison("traveltime")
plot_comparison("schoolsup")
plot_comparison("health")

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
mat$group <- "math"
por <- common_student[c("sex", "G1.y", "G2.y", "G3.y", "studytime.x")]
por$studytime <- por$studytime.x
por$studytime.x <- NULL
por$grad <- por$G3.y
por$G3.y <- NULL
por$G2 <- por$G2.y
por$G2.y <- NULL
por$G1 <- por$G1.y
por$G1.y <- NULL
por$group <- "por"
df <- rbind(mat, por)

##  PLOT
cbPalette <- c(  "#A60A55","#4577A0")
palet <- c(         "#ffd700",
                     "#fa8775",
                     "#cd34b5",
                     "#9d02d7",
                    "#CC79A7","#56B4E9")
                    

## scatter with final grade with gaussian noise
ggplot() +
  geom_jitter(aes(x=common_student$G3.y, y =common_student$G3.x, color = common_student$sex,  size=common_student$studytime_mean), alpha = 0.7, width = 0.4, height = 0.4) + 
  theme_light()+
  scale_color_manual(values = cbPalette)+
  geom_hline(yintercept=mean(common_student$G3.x),  linetype="dashed", color="#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=-0, y=mean(common_student$G3.x)), label="Mean math grade", vjust=1.4, color="#090809", alpha=0.4)+
  geom_vline(xintercept=mean(common_student$G3.y),  linetype="dashed", color= "#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=mean(common_student$G3.y), y=18), label="Mean portuguese grade", vjust=-1.2, color="#090809", angle=90, alpha=0.4)+
  ggtitle("Multi-Dimensional Analysis of Students' Grades, Sex, and Study Hours")+
  labs(caption = "Source: UCI Machine Learning Repository: Student Performance Data Set", color="Sex", size="Study hours")+
  theme(legend.position = c(0.93,0.10), legend.background = element_rect(fill="white",size=0.5, linetype="solid",colour ="#090800"), legend.box = "horizontal", axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))+
  ylab("Math Grade") +
  scale_y_continuous(limits = c(0, 20), oob = scales::squish)+
  scale_x_continuous(limits = c(0, 20), oob = scales::squish)+
  xlab("Spanish Grade")
ggsave("PLOT/scatter_grade_sex_study.jpeg", dpi=700, bg="white", width = 16, height = 16)



## SCatter with mean
ggplot() +
  geom_jitter(aes(x=common_student$mean_grade.y, y =common_student$mean_grade.x, color = common_student$sex,  size=common_student$studytime.x), alpha = 0.7, width = 0.4, height = 0.4) + 
  theme_light()+
  scale_color_manual(values = cbPalette)+
  #geom_hline(yintercept=mean(common_student[which(common_student$sex=="M"),]$G3.x),  linetype="dashed", color = "#4577A0", size=0.6, alpha=0.5 )+
  #geom_hline(yintercept=mean(common_student[which(common_student$sex=="F"),]$G3.x),  linetype="dashed", color="#A60A55", size=0.6, alpha=0.5)+
  #geom_vline(xintercept=mean(common_student[which(common_student$sex=="M"),]$G3.y),  linetype="dashed", color= "#4577A0", size=0.6, alpha=0.5)+
  #geom_vline(xintercept=mean(common_student[which(common_student$sex=="F"),]$G3.y),  linetype="dashed", color="#A60A55",size=0.6, alpha=0.5)+
  geom_hline(yintercept=mean(common_student$mean_grade.x),  linetype="dashed", color="#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=-0, y=mean(common_student$mean_grade.x)), label="Mean math grade", vjust=1.4, color="#090809", alpha=0.4)+
  geom_vline(xintercept=mean(common_student$mean_grade.y),  linetype="dashed", color= "#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=mean(common_student$mean_grade.y), y=18), label="Mean portuguese grade", vjust=-1.2, color="#090809", angle=90, alpha=0.4)+
  ggtitle("Multi-Dimensional Analysis of Students' Grades, Sex, and Study Hours")+
  labs(caption = "Source: UCI Machine Learning Repository: Student Performance Data Set", color="Sex", size="Study hours")+
  theme(legend.position = c(0.93,0.10), legend.background = element_rect(fill="white",size=0.5, linetype="solid",colour ="#090800"), legend.box = "horizontal")+
  ylab("Math Grade") +
  xlab("Spanish Grade")
ggsave("PLOT/scatter_grade_sex_study.jpeg", dpi=700, bg="white", width = 16, height = 10)






##beeswarm
library(ggbeeswarm)
ggplot() +
  geom_jitter(aes(x=df$group, y=df$grad, col=as.factor(df$sex), size=as.factor(df$studytime), alpha=0.8 ), width=0.14, height=0.4) +
  geom_violin(aes(x=df$group, y=df$grad, col=as.factor(df$sex), fill=as.factor(df$sex)), alpha=0.05, position = "identity")+
  scale_color_manual(name="Sex", values=cbPalette)+
  guides(fill= "none", size=guide_legend(title="Study time"), alpha="none")+
  xlab("Class") + ylab("Grade")+
  scale_fill_manual(values=cbPalette)+
  ggtitle("Multi-Dimensional Analysis of Students' Grades, Sex, and Study Hours")+
  labs(caption = "Source: UCI Machine Learning Repository: Student Performance Data Set")+
  theme_minimal()
ggsave("PLOT/violin_grade_sex_study.png", dpi=600, bg="white", width = 16, height = 9)

ggplot() +
  geom_jitter(aes(x=df$sex, y=df$grad, col=as.factor(df$group), size=as.factor(df$studytime), alpha=0.5 ), width=0.35, height=0.4) +
  geom_violin(aes(x=df$sex, y=df$grad, col=as.factor(df$group)), fill=NA, position = "identity")+
  scale_color_manual(values=palet)+
  theme_minimal()




## COLOR INTENSITY INSTEAD SIZE

library(ggnewscale)
cbPalette <- c(  "#A60A55","#4577A0")
palet <- c("#ffd700","#fa8775","#cd34b5","#9d02d7","#CC79A7","#56B4E9")
male_palette <- c("#afcae0","#88afd1","#6095c1","#387ab2")
female_palette <- c("#ce9cb4", "#b66b8f"  , "#9d3969"  , "#850844")

## scatteR
ggplot() +
  geom_jitter(aes(x=common_student[which(common_student$sex == "M"),]$G3.y, y =common_student[which(common_student$sex == "M"),]$G3.x, color =common_student[which(common_student$sex == "M"),]$studytime_mean),size=2.9, alpha = 1, width = 0.4, height = 0.4) + 
  scale_colour_gradientn(colours = c("#afcae0", "#387ab2"))+
  labs(color="Male study hours:")+
  new_scale_color()+
  geom_jitter(aes(x=common_student[which(common_student$sex == "F"),]$G3.y, y =common_student[which(common_student$sex == "F"),]$G3.x, color =common_student[which(common_student$sex == "F"),]$studytime_mean),size=2.9, alpha = 1, width = 0.4, height = 0.4) +
  scale_colour_gradientn(colours = c("#ce9cb4", "#850844"))+
  labs(color="Female study hours:")+
  theme_light()+
  geom_hline(yintercept=mean(common_student$G3.x),  linetype="dashed", color="#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=-0, y=mean(common_student$G3.x)), label="Mean math grade", vjust=1.4, color="#090809", alpha=0.4)+
  geom_vline(xintercept=mean(common_student$G3.y),  linetype="dashed", color= "#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=mean(common_student$G3.y), y=18), label="Mean portuguese grade", vjust=-1.2, color="#090809", angle=90, alpha=0.4)+
  ggtitle("Multi-Dimensional Analysis of Students' Grades, Sex, and Study Hours")+
  labs(caption = "Source: UCI Machine Learning Repository: Student Performance Data Set")+
  theme(legend.position = c(0.94,0.21), legend.background = element_rect(fill="white",size=0.5, linetype="solid",colour ="#090800"), legend.box = "vertical", axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))+
  ylab("Math Grade") +
  scale_y_continuous(limits = c(0, 20), oob = scales::squish)+
  scale_x_continuous(limits = c(0, 20), oob = scales::squish)+
  xlab("Spanish Grade")


## violin
ggplot() +
  geom_violin(aes(x=df$group, y=df$grad, col=as.factor(df$sex)), alpha=0.1, position = "identity")+
  scale_color_manual(name="Sex", values=cbPalette)+
  new_scale_color()+ 
  geom_jitter(aes(x=df[which(df$sex=="M"),]$group, y=df[which(df$sex=="M"),]$grad, col=as.factor(df[which(df$sex=="M"),]$studytime)), width=0.14, height=0.4,size=2.5) +
  scale_color_manual(name="Male studytime (hours)", values = male_palette)+
  new_scale_color()+
  geom_jitter(aes(x=df[which(df$sex=="F"),]$group, y=df[which(df$sex=="F"),]$grad, col=as.factor(df[which(df$sex=="F"),]$studytime)), width=0.14, height=0.4,size =2.5 ) +
  scale_color_manual(name="Female studytime (hours)",values = female_palette)+
  guides(fill= "none", size=guide_legend(title="Study time"), alpha="none")+
  xlab("Class") + ylab("Grade")+
  scale_fill_manual(values=cbPalette)+
  ggtitle("Multi-Dimensional Analysis of Students' Grades, Sex, and Study Hours")+
  labs(caption = "Source: UCI Machine Learning Repository: Student Performance Data Set")+
  theme_minimal()

