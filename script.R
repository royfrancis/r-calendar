# Script to create a photo calendar with a background images

# load libraries
library(ggplot2) # for plotting
library(stringr) # for text processing
library(Cairo) # for graphics
library(extrafont) # for custom fonts
library(png) # read png
library(jpeg) # read jpeg

# prepare fonts
font_import(pattern="Gidole",prompt=FALSE)
loadfonts(device = "win")
windowsFonts(sans="Gidole")

rm(list=ls())

img_bleed <- 6
img_height <- 297+(img_bleed*2)
img_width <- 420+(img_bleed*2)
img_dpi <- 300
img_units <- "mm"
export <- "png"

# set working directory
#setwd("D:/data/work/calendar2018")

# PREPARE CALENDAR DATA.FRAME
dates <- seq(as.Date("2018-01-01"),as.Date("2018-12-31"),"days")
df_raw <- data.frame(fulldate=as.character(dates),
                  date=as.numeric(strftime(dates,format="%d")),
                  month=as.numeric(strftime(dates,format="%m")),
                  year=strftime(dates,format="%Y"),
                  day=strftime(dates,format="%a"),
                  week=strftime(dates,format="%W"),
                  stringsAsFactors=F)

# read marked dates and description
marked <- read.delim("marked.txt",sep="\t",header=T,stringsAsFactors=F)
# add newline to last space in the description
marked$description <- str_replace(marked$description," [:alnum:]+$",str_replace(str_extract(marked$description," [:alnum:]+$")," ","\n"))
# create R dates
marked$fulldate <- as.character(strftime(as.Date(str_replace_all(paste0(marked$date," 2018")," ","-"),format="%b-%d-%Y"),format="%Y-%m-%d"))
# identify duplicate dates if any
marked[which(duplicated(marked$fulldate)),]
# remove character date
marked$date <- NULL

# merge calendar and marked dates
df_cal <- merge(df_raw,marked,by="fulldate",all.x=T)
# mark NA descriptions as not important
df_cal$type[is.na(df_cal$type)] <- "not important"
# assign red colour to public holidays and weekends
df_cal$holiday <- ifelse(df_cal$day=="Sat"|df_cal$day=="Sun"|df_cal$type=="public holiday","#e41a1c","grey20")
# assign blue colour to birthdays
df_cal$holiday[df_cal$type=="birthday"] <- "steelblue"
# mark start of week
df_cal$week <- ifelse(df_cal$day=="Mon",df_cal$week,NA)
# alpha of background shape for week
df_cal$weekbgalpha <- ifelse(is.na(df_cal$week),0,1)
# split description into two lines
df_cal$description <- paste0(df_cal$description,"\nx")
df_cal$description1 <- sapply(strsplit(df_cal$description,"\n"),"[[",1)
df_cal$description2 <- sapply(strsplit(df_cal$description,"\n"),"[[",2)
df_cal$description1[df_cal$description1=="NA"] <- NA
df_cal$description2[df_cal$description2=="x"] <- NA

# prepare image captions
cap <- read.delim("captions.txt",header=T,stringsAsFactors=F)

# set text position variables
basepos <- 0.35
df_cal$markpos1 <- basepos+0.08
df_cal$markpos2 <- df_cal$markpos1[1]+0.12
df_cal$datepos <- df_cal$markpos2[1]+0.20
df_cal$daypos <- df_cal$datepos[1]+0.25
df_cal$weekpos <- df_cal$daypos[1]+0.25
df_cal$monthpos <- df_cal$weekpos[1]+0.26

# EXPORT LOOP
# loop to create and export monthwise
levs <- levels(factor(df_cal$month))
for(i in 1:length(levs))
{
  fullmonth <- month.name[i]
  message(paste0("Running ",fullmonth," ..."))
  
  # subset one month
  single <- subset(df_cal,df_cal$month==i)
  
  # read background image
  #pic <- readPNG(paste0("./images/",i,".png"))
  pic <- readJPEG(paste0("./images/",i,".jpg"))
  pic1 <- grid::rasterGrob(pic)
  pic1$width <- unit(1, "npc") 
  pic1$height <- unit(1, "npc")
  
  # create final image
  p <- ggplot(single)+
    # background image
    annotation_custom(pic1)+
    # background rectangle 1
    annotate("rect",xmin=-Inf,xmax=Inf,ymin=0,ymax=single$weekpos[1],alpha=1,fill="white")+
    # background rectangle 2
    annotate("rect",xmin=-Inf,xmax=Inf,ymin=single$weekpos[1],ymax=single$weekpos[1]+0.4,alpha=0.6,fill="white")+
    # month title text faded with year
    annotate("text",x=1,y=single$monthpos[1],label=paste0(fullmonth," 2018"),size=7,hjust=0,col="grey40",fontface="bold")+
    # month title text
    annotate("text",x=1,y=single$monthpos[1],label=fullmonth,size=7,hjust=0,col="grey20",fontface="bold")+
    # image description text
    annotate("text",x=single$date[nrow(single)],y=single$monthpos[1],label=cap$description[i],size=4,hjust=1,col=cap$textcol[i])+
    # week background circle
    geom_point(aes(x=date,y=weekpos),size=11,colour="grey90",alpha=single$weekbgalpha,shape=20)+
    # week text
    geom_text(aes(x=date,y=weekpos),label=single$week,size=3,col="grey20",fontface="bold")+
    # day text
    geom_text(aes(x=date,y=daypos),label=single$day,size=4.8,col=single$holiday)+
    # date text
    geom_text(aes(x=date,y=datepos),label=single$date,size=7.5,col=single$holiday)+
    # important information text line 1
    geom_text(aes(x=date,y=markpos1),label=single$description2,size=2.8,col="grey20",vjust=1)+
    # important information text line 2
    geom_text(aes(x=date,y=markpos2),label=single$description1,size=2.8,col="grey20",vjust=1)+
    # limit scales
    scale_x_continuous(limits=c(0,max(single$date)+1),expand=c(0,0.6))+
    scale_y_continuous(limits=c(0,10),expand=c(0,0))+
    labs(x=NULL,y=NULL)+
    theme_bw(base_family="Gidole")+
    # remove graph elements
    theme(plot.background=element_rect(fill="transparent",colour=NA),
          plot.margin = margin(c(0,0,0,0)),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.ticks.length = unit(0,"pt"),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background=element_rect(fill="transparent",colour=NA),
          panel.ontop=TRUE)
  
  # export image
  if(export=="png")
  {
    png(filename=paste0(single$month[1],".png"),height=img_height,
         width=img_width,res=img_dpi,units=img_units,family="Gidole",
         bg="transparent",type="cairo")
    print(p)
    dev.off()
    message(paste0("Exported ",single$month[1],".png"))
  }
  
  if(export=="tiff")
  {
    tiff(filename=paste0(single$month[1],".tiff"),height=img_height,
              width=img_width,res=img_dpi,units=img_units,family="Gidole",
              compression="lzw",type="cairo",bg="transparent")
    print(p)
    dev.off()
    message(paste0("Exported ",single$month[1],".tiff"))
  }
  
  if(export=="pdf")
  {
    pdf(file=paste0(single$month[1],".pdf"),height=round(img_height*0.039,0),
        width=round(img_width*0.039,0),family="Gidole")
    print(p)
    dev.off()
    message(paste0("Exported ",single$month[1],".pdf"))
  }
}










