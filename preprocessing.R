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
palet <- c(         #"#ffd700",
                     "#fa8775",
                     "#cd34b5",
                     "#9d02d7",
                    "#CC79A7","#56B4E9")
## scatter
ggplot() +
  geom_jitter(aes(x=common_student$G3.y, y =common_student$G3.x, color = common_student$sex,  size=as.factor(common_student$studytime.x)), alpha = 0.7) + 
  theme_light()+
  scale_color_manual(values = cbPalette)+
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
  geom_jitter(aes(x=df$group, y=df$grad, col=as.factor(df$sex), size=as.factor(df$studytime), alpha=0.5 ), width=0.14, height=0.4) +
  geom_violin(aes(x=df$group, y=df$grad, col=as.factor(df$sex)), fill=NA, position = "identity")+
  scale_color_manual(values=cbPalette)+
  theme_minimal()


ggplot() +
  geom_jitter(aes(x=df$sex, y=df$grad, col=as.factor(df$group), size=as.factor(df$studytime), alpha=0.5 ), width=0.14, height=0.4) +
  geom_violin(aes(x=df$sex, y=df$grad, col=as.factor(df$group)), fill=NA, position = "identity")+
  scale_color_manual(values=palet)+
  theme_minimal()

