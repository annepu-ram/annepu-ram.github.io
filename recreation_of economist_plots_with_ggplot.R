ifelse(require(splitstackshape),{library("splitstackshape")},install.packages("splitstackshape"))
ifelse(require(reshape2),{library("reshape2")},install.packages("reshape2"))
ifelse(require(ggplot2),{library("ggplot2")},install.packages("ggplot2"))
ifelse(require(scales),{library("scales")},install.packages("scales"))
ifelse(require(magrittr),{library("magrittr")},install.packages("magrittr"))
ifelse(require(dplyr),{library("dplyr")},install.packages("dplyr"))
ifelse(require(zoo),{library("zoo")},install.packages("zoo"))
ifelse(require(showtext),{library("showtext")},install.packages("showtext"))
ifelse(require(ggthemes),{library("ggthemes")},install.packages("ggthemes"))
library(grid)
#library(showtext)

font_add("ITC Officina Book", "itc-officina-sans-std-book-58abde7814311.otf")

font_add("ITC Officina Book Italic", "itc-officina-sans-std-book-italic-58abdf2354251.otf")

font_add("ITC Officina Bold","itc-officina-sans-std-bold-58abdee9ae14e.otf")

font_add("ITC Officina Bold Italic", "itc-officina-sans-std-bold-italic-58abdeff1b0ec.otf")

showtext.auto()

glb_temp1<-read.csv(file.choose(), sep = "\t",header = F)

glb_df1<-cSplit(glb_temp1, "V1", "\\s+", direction = "wide",stripWhite=TRUE,fixed = F, type.convert=FALSE)


glb_temp2<-read.csv(file.choose(), sep = "\t",header = F)

glb_df2<-cSplit(glb_temp2, "V1", "\\s+", direction = "wide",stripWhite=TRUE,fixed = F, type.convert=FALSE)


glb_temp3<-read.csv(file.choose(), sep = "\t",header = F)

glb_df3<-cSplit(glb_temp3, "V1", "\\s+", direction = "wide",stripWhite=TRUE,fixed = F, type.convert=FALSE)

glb_df<-rbind(glb_df1,glb_df2,glb_df3)

glb_df<-glb_df[,-c(14:20)]

glb_df<-glb_df[-c(23,44),]

names(glb_df)<-as.character(c("year",as.character(c(1:12))))


glb_df<-glb_df[-1,] #remove first row

glb_df<-melt(glb_df,id.vars=1) #reshape data columns into rows to get time series

glb_df<-glb_df[order(glb_df[,1], glb_df[,2]), ] #order by year and months 

row.names(glb_df)<-1:nrow(glb_df)

#remove last rows with no data

glb_df<-glb_df[-c(711:720),]

glb_df$value<-as.numeric(glb_df$value)*.01

glb_df$Date <- zoo::as.yearmon(paste(glb_df$year,glb_df$variable), "%Y %m")

glb_df$Date<- zoo::as.Date.yearmon(glb_df$Date)

annual_Mean <- glb_df %>%
  group_by(year) %>%
  select(year, value) %>%
  summarise(Mean = mean(value))

annual_Mean<-annual_Mean[-60,]

predicted_values<-read.csv(file.choose(),header = F,sep = "\t")

predicted_values<-cSplit(predicted_values, "V1", "\\s+", direction = "wide",stripWhite=TRUE,fixed = F, type.convert=FALSE)

predicted_values<-predicted_values[,-c(seq(2,4),6)]

names(predicted_values)<-c("year","Mean")

annual_Mean<-as.data.frame(annual_Mean)

predicted_values<-as.data.frame(predicted_values)

annual_Mean<-rbind(annual_Mean, predicted_values)

annual_Mean<-as.data.frame(annual_Mean)

annual_Mean$Mean<-as.single(annual_Mean$Mean)

annual_Mean$Mean<-round(annual_Mean$Mean,2)

annual_Mean$year<-as.Date(ISOdate(annual_Mean$year, 1, 1))

annual_Mean$y<-1:nrow(annual_Mean)

# pred <- predict(lm(Mean ~ y, annual_Mean), 
#                 se.fit = TRUE, interval = "confidence")
# 
# limits <- as.data.frame(pred$fit)
# 
# limits$year<-annual_Mean$year


  
ggplot(glb_df,
       aes(x=Date,
           y=value))+
  
  geom_line(color="#7b90b2")+
  
  geom_hline(yintercept = 1.5,color="#ed891e",size=1)+
  
  scale_x_date(name="",limits = as.Date(c("1948-01-01","2100-01-01")),date_breaks = "20 years",minor_breaks="10 years",labels = date_format("%Y"))+
  
  geom_smooth(data=annual_Mean,method='lm',mapping=aes(x=year,y=Mean),formula=y ~ poly(x, 4),fullrange=F,col="#ff8738",size=1,level=0.99,fill = "#6095ea",linetype="dashed")+
  
  geom_smooth(method='lm',formula=y ~ poly(x, 3),fullrange=F,col="#f2a571",size=1,level=0.999999,fill="#ed891e",linetype=0)+
  
  geom_segment(aes(x=as.Date("2017-01-01"), xend=as.Date("2017-01-01"),
                   y = -0.25, yend = 1.2), col="grey")+
  
  annotate("text",x=as.Date("2017-01-01"),
                        y = 0.25,label=2017, size=3)+
  
  geom_segment(aes(x=as.Date("2055-01-01"), xend=as.Date("2055-01-01"),
                   y = -0.25, yend = 1.45), col="grey")+
  
  annotate("text",x=as.Date("2055-01-01"),
           y = 0.5,label=2055, size=3)+
  
  scale_y_continuous(name="",limits = c(-0.25, 2.0),
                     breaks = seq(0.0, 2.0, by = 0.5),position = "right")+
  
  annotate("text",x = as.Date("1960-01-01"), y = .25, label = "Estimated human caused \n warming to date,\n and likely range",col="#ed891e",size=4,family="Serif bold text")+
  
  annotate("text" ,x = as.Date("1997-01-01"), y = 1.25, label = "Observed monthly global \n mean surface temperature",col="#7b90b2",size=4,family="Serif bold text")+
  
  annotate("text" ,x = as.Date("2055-01-01"), y = 2, label = "likely temperature change under scenarios\n where carbon emissions reach net-zero in 2055",col="#6095ea",size=4,family="Serif text")+
  
  labs(title = "Aim Lower",
       subtitle = "Global warming relative to 1850-1900, \u00B0C",
       caption = "Source: IPCC") +
  
  theme_minimal() + # start with a minimal theme and add what we need
  
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # position the legend in the upper left 
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text.x = element_text(face = "bold",),
        plot.title = element_text(size=14,  family="Sans Bold text"),
        plot.subtitle = element_text(size=9,  family="Sans Plain text"),
        plot.caption = element_text(size=8,hjust = 0,color="grey40"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(colour = "black",size = 1),
        panel.grid.major.y  = element_line(color = "#c0cde2", size = 1),
        panel.grid.minor.y  = element_line(color = "#c0cde2", size = 1),
        panel.grid.major.x= element_blank(), panel.grid.minor.x = element_blank()
  )


################################# graph for carbon emissions

co2_emi<- read.csv(file.choose(), header = TRUE, stringsAsFactors = F)
co2_emi<-as.data.frame(co2_emi)
co2_emi$X2016<-as.single(co2_emi$X2016)
co2_emi$X2017<-as.single(co2_emi$X2017)

co2_emi$difference<-as.single(co2_emi$`X2017`-co2_emi$`X2016`)

co2_emi<-na.omit(co2_emi)

co2_emi<-co2_emi[order(co2_emi$difference),]

co2_emi_signif<-co2_emi %>% top_n(5,difference)

co2_emi_neg<-co2_emi %>% top_n(-5,difference)

co2_emi_signif <- rbind(co2_emi_neg,co2_emi_signif)

co2_emi_signif <- transform(co2_emi_signif, Million.tonnes.of.carbon.dioxide = reorder(Million.tonnes.of.carbon.dioxide, -difference))

#rownames(co2_emi_signif)<-1:nrow(co2_emi_signif)

text_reductions <- textGrob("Largest reductions", gp=gpar(fontsize=11, fontface="bold"))
text_increases <- textGrob("Largest increases", gp=gpar(fontsize=11, fontface="bold"))


p<-ggplot(co2_emi_signif[order(co2_emi_signif$difference),], aes(x=Million.tonnes.of.carbon.dioxide, y=difference)) + 
  
  geom_bar(stat='identity', width = 0.6, position = position_dodge(width=0.4),fill="#2a75a8")  +
  
  geom_hline(yintercept = 0,color="#ed891e",size=1)+
  
  theme_economist() +
  
  scale_color_economist()+
  
  scale_y_continuous(name="", limits = c(-45,120), breaks=seq(-40,120,by=40),position = "right")+
  
  scale_x_discrete(name="")+
  
  #scale_fill_economist()+
  
  labs(title="A big, beautiful fall ", 
       subtitle=expression(paste("Change in CO"[2]," emissions, 2016-17, tonnes m")),
       caption = "Source: BP Statistical Review of world Energy, 2018") +
  
  theme(text = element_text(color = "gray20"),
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        #axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size=12,hjust = 0),
        plot.subtitle = element_text(size=9,hjust = 0),
        plot.caption = element_text(size=8,hjust = 0,color="grey50"),
        # axis.title.x = element_text(vjust = -1), # move title away from axis
        # axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        # axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.x = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.ticks.x = element_line(color = "#c0cde2", size = 1),
        panel.grid.major.x  = element_line(color = "#c0cde2", size = 1),
        panel.grid.minor.x  = element_blank(),
        panel.grid.major.y= element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.y=element_text(margin = margin(t = 5))
        
  )+
   annotation_custom(text_reductions,xmin=10,xmax=11,ymin=-70,ymax=-50) + 
   annotation_custom(text_increases,xmin=5.2,xmax=5.8,ymin=-70,ymax=-50 )+
  coord_flip()

g = ggplotGrob(p)
g$layout$clip[g$layout$name=="panel"] <- "off"
grid.draw(g)








########################################################## my twist to the actuall plot

co2_emi_signif$sign<-ifelse(co2_emi_signif$difference<0,"below","above")

ggplot(co2_emi_signif, aes(x=`Million.tonnes.of.carbon.dioxide`, y=difference, label=round(difference,1)) )+ 
  
  geom_segment(aes(y = 0, 
                   x = `Million.tonnes.of.carbon.dioxide`, 
                   yend = difference, 
                   xend = `Million.tonnes.of.carbon.dioxide`, 
               col=sign),size=1.5)+
  
  geom_point(stat='identity', aes(col=`sign`), size=9)  +
  
  scale_y_continuous(name="", limits = c(-45,120), breaks=seq(-40,120,by=40),position = "right")+
  
  scale_x_discrete(name="")+
  
  scale_color_manual(name="change in emisisons", 
                     labels = c("Greatest Increases", "greatest decreases"), 
                     values = c("above"="#f8766d", "below"="#00ba38")) + 
  geom_text(color="white", size=3)+
  
  labs(title="A big, beautiful fall.", 
       subtitle=expression(paste("Change in CO"[2]," emissions, 2016-17, tonnes m.")),
       caption = "Source: BP Statistical Review of world Energy, 2018")+
  
  theme(text = element_text(color = "gray20"),
        panel.background = element_rect(fill = "#ffe8b7",
                                        colour = "#ffe8b7",
                                        size = 0.5, linetype = "solid"),
        plot.background = element_rect(fill = "#ffe8b7"),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "#ffe8b7",
                                         colour = "#ffe8b7",
                                         size = 0.5, linetype = "solid"),
        legend.box.background = element_rect(fill = "#ffe8b7",
                                             colour = "#ffe8b7",
                                             size = 0.5, linetype = "solid"),
        legend.position = "top",
        legend.justification = 1, # anchor point for legend.position.
        legend.text = element_text(size = 9, color = "gray10",face="bold"),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
  
        legend.title = element_text(size = 9, color = "gray10", face="bold" ),
        #axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size=12,hjust = 0,face="bold"),
        plot.subtitle = element_text(size=9,hjust = 0,face="bold"),
        plot.caption = element_text(size=8,hjust = 0,color="grey50"),
        # axis.title.x = element_text(vjust = -1), # move title away from axis
        # axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        # axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.x = element_blank(),
        axis.text.y=element_text(face = "bold",margin = margin(t = 5)),
        axis.ticks.y.right = element_blank(),
        #axis.ticks.x = element_line(color = "#c0cde2", size = 1),
        panel.grid.major.x  = element_line(color = "black", size = 1,linetype = "dashed"),
        panel.grid.minor.x  = element_blank(),
        panel.grid.major.y= element_blank(), panel.grid.minor.y = element_blank()
        
        
  )+
  annotate("text",x=10.5,y=100,label="italic(annepu-ram.github.io)",col="grey50",size=4,parse=TRUE)+ #branding the graph
  coord_flip()


  








