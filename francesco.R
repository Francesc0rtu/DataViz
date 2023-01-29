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

data = common_student

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
## scatter
ggplot() +
  geom_jitter(aes(x=common_student$G3.y, y =common_student$G3.x, color = common_student$sex,  size=as.factor(common_student$studytime.x)), alpha = 0.7) + 
  theme_light()+
  scale_color_manual(values = cbPalette)+
  #geom_hline(yintercept=mean(common_student[which(common_student$sex=="M"),]$G3.x),  linetype="dashed", color = "#4577A0", size=0.6, alpha=0.5 )+
  #geom_hline(yintercept=mean(common_student[which(common_student$sex=="F"),]$G3.x),  linetype="dashed", color="#A60A55", size=0.6, alpha=0.5)+
  #geom_vline(xintercept=mean(common_student[which(common_student$sex=="M"),]$G3.y),  linetype="dashed", color= "#4577A0", size=0.6, alpha=0.5)+
  #geom_vline(xintercept=mean(common_student[which(common_student$sex=="F"),]$G3.y),  linetype="dashed", color="#A60A55",size=0.6, alpha=0.5)+
  geom_hline(yintercept=mean(common_student$G3.x),  linetype="dashed", color="#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=-0, y=mean(common_student$G3.x)), label="Mean math grade", vjust=1.4, color="#090809", alpha=0.4)+
  geom_vline(xintercept=mean(common_student$G3.y),  linetype="dashed", color= "#090809", size=0.6, alpha=0.4)+
  geom_text(aes(x=mean(common_student$G3.y), y=18), label="Mean portuguese grade", vjust=-1.2, color="#090809", angle=90, alpha=0.4)+
  ggtitle("Multi-Dimensional Analysis of Students' Grades, Sex, and Study Hours")+
  labs(caption = "Source: UCI Machine Learning Repository: Student Performance Data Set", color="Sex", size="Study hours")+
  ylab("Math Grade") +
  xlab("Spanish Grade")


## box
grid.arrange(
ggplot(data = common_student) + geom_boxplot(aes(x=sex, y=G3.x, fill=sex)) +
  scale_fill_manual(values = cbPalette)+
  theme_minimal()+
  ylab("Math grade"),
 ggplot(data = common_student) + geom_boxplot(aes(x=sex, y=G3.y, fill=sex))+
  scale_fill_manual(values = cbPalette)+
  theme_minimal()+
  ylab("Pourt grade"),

   ncol=2
)

##violin
grid.arrange(
  ggplot(data = common_student) + geom_violin(aes(x=sex, y=G3.x, fill=sex)) +
  theme_minimal()+
    scale_fill_manual(values=cbPalette)+
  ylab("Math grade"),
  ggplot(data = common_student) + geom_violin(aes(x=sex, y=G3.y, fill=sex))+
  theme_minimal()+
    scale_fill_manual(values=cbPalette)+
  ylab("Pourt grade"),
  ncol=2
)


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


ggplot() +
  geom_jitter(aes(x=df$sex, y=df$grad, col=as.factor(df$group), size=as.factor(df$studytime), alpha=0.5 ), width=0.35, height=0.4) +
  geom_violin(aes(x=df$sex, y=df$grad, col=as.factor(df$group)), fill=NA, position = "identity")+
  scale_color_manual(values=palet)+
  theme_minimal()




## other violin
ggplot()+
  geom_violin(aes(x=as.factor(df$studytime), y=df$grad, fill=df$group, ), alpha=0.2, colour = NA)+
  geom_jitter(aes(x=0.77, y=df[which( df$studytime==1 & df$group=="math"),]$grad, col=df[which( df$studytime==1 & df$group=="math"),]$sex), width=0.1, height = 0.15, size=2, alpha=0.7)+
  geom_jitter(aes(x=1.22, y=df[which( df$studytime==1 & df$group=="por"),]$grad, col=df[which( df$studytime==1 & df$group=="por"),]$sex), width=0.1, height = 0.15, size=2, alpha=0.7)+
  geom_jitter(aes(x=1.77, y=df[which( df$studytime==2 & df$group=="math"),]$grad, col=df[which( df$studytime==2 & df$group=="math"),]$sex), width=0.1, height = 0.15, size=2, alpha=0.7)+
  geom_jitter(aes(x=2.22, y=df[which( df$studytime==2 & df$group=="por"),]$grad, col=df[which( df$studytime==2 & df$group=="por"),]$sex), width=0.1, height = 0.15, size=2, alpha=0.7)+
  geom_jitter(aes(x=2.77, y=df[which( df$studytime==3 & df$group=="math"),]$grad, col=df[which( df$studytime==3 & df$group=="math"),]$sex), width=0.1, height = 0.15, size=2, alpha=0.7)+
  geom_jitter(aes(x=3.22, y=df[which( df$studytime==3 & df$group=="por"),]$grad, col=df[which( df$studytime==3 & df$group=="por"),]$sex), width=0.1, height = 0.15, size=2, alpha=0.7)+
  geom_jitter(aes(x=3.77, y=df[which( df$studytime==4 & df$group=="math"),]$grad, col=df[which( df$studytime==4 & df$group=="math"),]$sex), width=0.1, height = 0.15, size=2, alpha=0.7)+
  geom_jitter(aes(x=4.22, y=df[which( df$studytime==4 & df$group=="por"),]$grad, col=df[which( df$studytime==4 & df$group=="por"),]$sex), width=0.1, height = 0.15, size=2, alpha=0.7)+
  theme_minimal()+
  theme()+
  guides(fill=guide_legend(title="Class"), col=guide_legend())+
  scale_color_manual(name="Sex", values=c("#cd34b5","#4577A0", '#8da0cb','#e78ac3', '#292F36','#698F3F'))+ 
  scale_fill_manual(values=c('#ffd700','#698F3F'))+
  xlab("Study time") +
  ylab("Final Grade")
