library(ggridges)
library(ggplot2)
library(dplyr)
library(forcats)

df<-read.csv("DATA/merge.csv")

attach(df)


df_sup_m4<-data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("x",'sup_m', "G1_m",'G2_m','G3_m'))))

supp_m_yes<-subset(df, (famsup_m=='yes' | paid_m=='yes' | schoolsup_m=="yes"))
supp_m_yes$famsup_m='yes'
supp_m_no<-subset(df,(paid_m=='no' & schoolsup_m=="no" & famsup_m=='no'))
supp_m_no$famsup_m='no'

df_sup_m4<-rbind(select(supp_m_yes,X,famsup_m,G1_m, G2_m, G3_m), df_sup_m4)
df_sup_m4<-rbind(select(supp_m_no,X,famsup_m,G1_m, G2_m, G3_m), df_sup_m4)


colnames(df_sup_m4)[2]<-"support"

df_sup_m5<-data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("x",'sup_m', "period",'Grade'))))


df_sup_m4['period']='First period grade'
df_sup_m5<-rbind(select(df_sup_m4,X,support, period,G1_m), df_sup_m5)
df_sup_m4['period']='Second period grade'
colnames(df_sup_m5)[4]<-"G2_m"
df_sup_m5<-rbind(select(df_sup_m4,X,support, period,G2_m), df_sup_m5)


df_sup_m4['period']='Final period grade'
colnames(df_sup_m5)[4]<-"G3_m"
df_sup_m5<-rbind(select(df_sup_m4,X,support, period,G3_m), df_sup_m5)

colnames(df_sup_m5)[4]<-"Grade"





med_df_sup_m5 <- df_sup_m5 %>%
  group_by(period, support) %>%
  summarize(media=mean(Grade))

media_yes <- data.frame(Species = c(1, 2, 3),
                        x0 = c(10.35, 10.62, 10.67 ))
media_no <- data.frame(Species = c(1, 2, 3),
                       x0 = c(10.48, 10.99, 11.45))


cbPalette <- c('#7ec072', '#ed989f')

df_sup_m5%>%
  ggplot(aes(x=Grade, y=fct_relevel(period,rev), group=interaction(period,support),color =support, 
             fill=support, height =after_stat(density)))+
  geom_density_ridges(scale = 0.95, stat = "density", alpha=0.4)+
  scale_fill_manual(values = cbPalette)+
  scale_color_manual(values = cbPalette)+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, .50)))+
  geom_segment(data = media_yes, 
               aes(x = x0, xend =x0, 
                   y = as.numeric(Species), yend = as.numeric(Species) + c(.895,.83,.90)),
               color = "#4577A0", inherit.aes = F)+
  geom_segment(data = media_no, 
               aes(x = x0, xend =x0, 
                   y = as.numeric(Species), yend = as.numeric(Species) + c(.68,.735,.71)),
               color = "#A60A55", inherit.aes = F)+
  #coord_cartesian(clip = "off")+
  ylab('Period') +
  xlab("Math grade")+
  labs(fill="Support:", color="Support:")+
  ggtitle("Distribution of math grades per type of support")+
  guides(fill = guide_legend(reverse=TRUE), color = guide_legend(reverse=TRUE ))+
  labs(caption = "Source: UCI Machine Learning Repository: Student Performance Data Set")+
  theme_minimal(base_size = 12)+
  #theme_ridges(center_axis_labels = TRUE)+
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(vjust=-1,size=12,color = "black"),
        plot.title = element_text(size=17.45, hjust = 0, face = "bold"),
        axis.text = element_text(size = 12, color="grey40"),
        axis.text.y = element_text(  color="black"),
        plot.caption=element_text(color = "grey55"),
        plot.subtitle = element_textbox_simple(
          color = "grey40", 
          size = 10.8,
          lineheight = 1.3, 
          margin = margin(t = 5, b = 30) ),
        legend.title = element_text(size=12),
        legend.text = element_text(
          color = "grey40", 
          size = 10.8
        ),
        legend.position = "top") 
  
ggsave("/Users/anna/Desktop/Data visualization/student/density_grade_support_period_no.jpeg", dpi=700, bg="white", width = 16, height = 10)





