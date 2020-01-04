#################### Packages ####################
library(ggplot2)
library(shiny)
library(magrittr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(TTR)
library(gqlr)
library(plotly)
library(kableExtra)
library(ggthemes)
pacman::p_load(glmnet)
library(matlab)

################### International Fund ###################
com <- read.csv("FIVFX.csv")
com$Date <- as.Date(as.character(com$Date))
log <- read.csv("log.csv")
log$Date <- as.Date(as.character(log$Date))
com1 <- read.csv("FHKCX.csv")
com1$Date <- as.Date(as.character(com1$Date))
log1 <- read.csv("FHKCXr.csv")
log1$Date <- as.Date(as.character(log1$Date))
com2 <- read.csv("FWWFX.csv")
com2$Date <- as.Date(as.character(com2$Date))
log2 <- read.csv("FWWFXr.csv")
log2$Date <- as.Date(as.character(log2$Date))
com3 <- read.csv("FIVLX1.csv")
com3$Date <- as.Date(as.character(com3$Date))
log3 <- read.csv("FIVLXr1.csv")
log3$Date <- as.Date(as.character(log3$Date))
com4 <- read.csv("FGIRX.csv")
com4$Date <- as.Date(as.character(com4$Date))
log4 <- read.csv("FGIRX_log_return.csv")
log4$Date <- as.Date(as.character(log4$Date))

# log4 <- read.csv("FRESX_Return.csv")
# log4$Date <- as.Date(as.character(log4$Date))
#############################################################

################################################## Sector Fund Staff #############################
Full_weight <-read.csv("Full_weight_Sector.csv")

Full_weight_FRESX <- Full_weight %>% 
  dplyr::filter(sector %in% "Real Estate")

Full_weight_FSMEX <- Full_weight %>% 
  dplyr::filter(sector %in% "Health Care")

Full_weight_FSPCX <- Full_weight %>% 
  dplyr::filter(sector %in% "Insurance")

Full_weight_FSPTX <- Full_weight %>% 
  dplyr::filter(sector %in% "Info Tech")

Full_weight_FSCPX <- Full_weight %>% 
  dplyr::filter(sector %in% "Consumer Discretionary")

######################### FRESX ##########################

FRESX_Return <- read.csv("FRESX_Return.csv")
FRESX_Return <- FRESX_Return %>%
  dplyr::select(-X) %>%
  mutate(Date=as.Date(Date))
FRESX_Return <- na.omit(FRESX_Return)

composite1 <- FRESX_Return%>%
  dplyr::select(-Date,-FRESX_Log_Return,-DWRSF_Log_Return)%>%
  as.matrix()

FRESX_Lasso <- FRESX_Return%>%
  dplyr::select(FRESX_Log_Return)%>%
  as.matrix()

######################### FSMEX ##########################
FSMEX_Return <- read.csv("FSMEX_Return.csv")
FSMEX_Return <- FSMEX_Return %>%
  dplyr::select(-X)

composite2 <- FSMEX_Return %>%
  dplyr::select(-Date, -FSMEX, -VHT)%>%
  as.matrix()

FSMEX_Lasso <- FSMEX_Return%>%
  dplyr::select(FSMEX)%>%
  as.matrix()

######################### FSPCX ##########################
FSPCX_Return <- read.csv("FSPCX_Return.csv")
FSPCX_Return <- FSPCX_Return %>%
  dplyr::select(-X)

composite3 <- FSPCX_Return %>%
  dplyr::select(-Date,-FSPCX,-FNCL) %>%
  as.matrix()

FSPCX_Lasso <- FSPCX_Return %>%
  dplyr::select(FSPCX) %>%
  as.matrix()

# ######################### FSPTX ##########################
# FSPTX_Return <- read.csv("FSPTX_Return.csv")
# 
# composite4 <- FSPTX_Return %>%
#   dplyr::select(-Date,-FSPTX,-MSCI_Info) %>%
#   as.matrix()
# 
# FSPTX_Lasso <- FSPCX_Return %>%
#   dplyr::select(FSPTX) %>%
#   as.matrix()

########################## FSCPX #########################

#################################### End of Sector Fund############################################ 

################### Bond Fund ##################################
#all_data<-read.csv("alldata.csv")
library(ggplot2)
################### add new plot##################################

pacman::p_load(tidyverse, plotly, ggplot2, RPMG, RColorBrewer, viridis, scales)
##1. Build Fidelity colour:   
fidelity.col <- c("#288dc1", "#799900", "#e89719", "#d0746f", "#94c6e0",  "#5d87a1", "#429080", "#c2a204", "#b45340", "#4a7628", "#4c5f6c", "#b4cc95", "#dc8633", "#477718", "#e6b17f", "#7a9a01", "#755372", "#477227", "#c19943", "#d89d8a", "#c8cfd3", "#977f96", "#ffffff", "#bccc80")
##--------------
##2. Set default theme:
theme_set(theme_minimal())

##--------------
##3. Set default colour:
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = fidelity.col)
}

scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = fidelity.col)
}

scale_colour_continuous<- function(...) {
  scale_color_gradient2(low = "#288dc1", high = "#4a7628", mid = "#e89719")
}

scale_fill_continuous<- function(...) {
  scale_fill_gradient2(low = "#288dc1", high = "#4a7628", mid = "#e89719")
}


com<-read.csv("MyData.csv")
com$Date<-as.Date(com$Date)
n = ggplot(com)+geom_line(aes(x = Date,y = Holding_Period_Return,color = type))+ 
  scale_x_date(limits = as.Date(c("2014-11-13","2019-11-01")),breaks = function(x) seq.Date(from = min(x),to = max(x), by = "1 years")) +
  ggtitle("FMSFX")+ xlab("")
plot.JYP <- plotly_build(n)

com_FBN<-read.csv("MyData_FBN.csv")

com_FBN$Date<-as.Date(com_FBN$Date)
fbndx = ggplot(com_FBN)+geom_line(aes(x = Date,y = Holding_Period_Return,color = type))+
  scale_x_date(limits = as.Date(c("2014-11-13","2019-11-01")),breaks = function(x) seq.Date(from = min(x),to = max(x), by = "1 years")) + ggtitle("FBNDX")+ xlab("")

ggplotly(fbndx)


com_FMNDX<-read.csv("MyData_FMNDX.csv")
com_FMNDX$Date<-as.Date(com_FMNDX$Date)
fmndx = ggplot(com_FMNDX)+geom_line(aes(x = Date,y = Holding_Period_Return,color = type))+
  scale_x_date(limits = as.Date(c("2014-11-13","2019-11-01")),breaks = function(x) seq.Date(from = min(x),to = max(x), by = "1 years")) + ggtitle("FMNDX")+ xlab("")
ggplotly(fmndx)


com_SPHIX<-read.csv("MyData_SPHIX.csv")
com_SPHIX$Date<-as.Date(com_SPHIX$Date)
sphix = ggplot(com_SPHIX)+geom_line(aes(x = Date,y = Holding_Period_Return,color = type))+
  scale_x_date(limits = as.Date(c("2014-11-13","2019-11-01")),breaks = function(x) seq.Date(from = min(x),to = max(x), by = "1 years")) + ggtitle("SPHIX")+ xlab("")


com_FCSTX<-read.csv("MyData_FCSTX.csv")
com_FCSTX$Date<-as.Date(com_FCSTX$Date)
fcstx = ggplot(com_FCSTX)+geom_line(aes(x = Date,y = Holding_Period_Return,color = type))+ 
  scale_x_date(limits = as.Date(c("2014-11-13","2019-11-01")),breaks = function(x) seq.Date(from = min(x),to = max(x), by = "1 years")) + ggtitle("FCSTX")+ xlab("")
ggplotly(fcstx)

data1<-read.csv("data1.csv")
fbn<-ggplot(data1,aes(x=Fund_FBNDX,y=Weights))+
  geom_bar(stat = "identity",aes(fill = Fund_FBNDX))+theme(legend.text=element_text(size=10),axis.text.x  = element_text(angle=25, hjust=1, vjust=0.9))
ggplotly(fbn)

com_FMS<-read.csv("MyData_FMS.csv")
fms<-ggplot(com_FMS,aes(x=Fund_FMSFX,y=Weights))+
  geom_bar(stat = "identity",aes(fill = Fund_FMSFX))+theme(legend.text=element_text(size=10),axis.text.x  = element_text(angle=25, hjust=1, vjust=0.9))
ggplotly(fms)


com_fmn<-read.csv("MyData_FMN.csv")
fmn<-ggplot(com_fmn,aes(x=Fund_FMNDX,y=Weights))+
  geom_bar(stat = "identity",aes(fill = Fund_FMNDX))+theme(legend.text=element_text(size=10),axis.text.x  = element_text(angle=25, hjust=1, vjust=0.9))

ggplotly(fmn)

com_sph<-read.csv("MyData_SPH.csv")
sph<-ggplot(com_sph,aes(x=com_sph$Fund_SPHIX,y=Weights))+
  geom_bar(stat = "identity",aes(fill = Fund_SPHIX))+theme(legend.text=element_text(size=10),axis.text.x  = element_text(angle=25, hjust=1, vjust=0.9))
#ggplotly(sph)

com_fcx<-read.csv("MyData_FCX.csv")
fcx<-ggplot(com_fcx,aes(x=Fund_FCSTX,y=Weights))+
  geom_bar(stat = "identity",aes(fill = Fund_FCSTX))+theme(legend.text=element_text(size=10),axis.text.x  = element_text(angle=25, hjust=1, vjust=0.9))

fcx

#############################################
data<-read.csv('alldata.csv')
data<-data.frame(data)




data$Date <- as.Date(data$Date, "%m/%d/%y")

SPHIX<-ggplot(data, aes(x=Date)) + 
  geom_line(aes(y=SPHIX, color="SPHIX")) +
  geom_line(aes(y=MLUHYCL.PI., color="MLUHYCLPI")) + 
  labs(color="Legend text")+
  ylab("Value")
ggplotly(SPHIX)
b<-ggplot(data, aes(x=Date)) + 
  geom_line(aes(y=data$LHMUN1Y.P., color="LHMUN1YP")) +
  geom_line(aes(y=FMNDX, color="FMNDX")) + 
  labs(color="Legend text")+
  ylab("Value")
ggplotly(b)
c<-ggplot(data, aes(x=Date)) + 
  geom_line(aes(y=FBNDX, color="FBNDX")) +
  geom_line(aes(y=data$LBUSTRUU.Index.x, color="LBUSTRUU")) + 
  labs(color="Legend text")+
  ylab("Value")
ggplotly(c)
d<-ggplot(data, aes(x=Date)) + 
  geom_line(aes(y=FCSTX, color="FCSTX")) +
  geom_line(aes(y=data$LMBITR.Index , color="LMBITR")) + 
  labs(color="Legend text")+
  ylab("Value") +
  theme_bw()
ggplotly(d)
e<-ggplot(data, aes(x=Date)) + 
  geom_line(aes(y=data$LFRAMBS.P., color="LFRAMBS")) +
  geom_line(aes(y=FMSFX, color="FMSFX")) + 
  labs(color="Legend text")+
  ylab("Value")
ggplotly(e)


####################### End of Bond Fund Staff ########################## 

shinyServer(function(input, output) {
  ###########International fund
  output$text <- renderUI(
    if(input$choose1 == "FWWFX"){
      includeHTML("FWWFX.Rhtml")
    }
    else if(input$choose1 == "FIVFX"){
      includeHTML("FIVFX.Rhtml")
    }
    else if(input$choose1 == "FIVLX"){
      includeHTML("FIVLX.Rhtml")
    }
    else if(input$choose1 == "FIGRX"){
      includeHTML("FIGRX.Rhtml")
    }
    else if(input$choose1 == "FWWFX"){
      includeHTML("FWWFX.Rhtml")
    }
    else if(input$choose1 == "FHKCX"){
      includeHTML("FHKCX.Rhtml")
    }
  )
  output$graph <- renderPlotly({
    if(input$choose1 == "FWWFX"){
     a <- ggplotly(ggplot(com2)+
                 geom_line(aes(x=Date, y=10000*(FWWFX/com2$FWWFX[1]), color = "FWWFX")) +
                 geom_line(aes(x=Date, y=10000*(MSCI_world/com2$MSCI_world[1]), color = "MSCI_world"))+
                 geom_line(aes(x=Date, y=10000*(NASDAQ/com2$NASDAQ[1]), color = "NASDAQ"))+
                 geom_line(aes(x=Date, y=10000*(SP_500/com2$SP_500[1]), color = "SP_500")) +
                 geom_line(aes(x=Date, y=10000*(CWI/com2$CWI[1]), color = "CWI")) +
                 geom_line(aes(x=Date, y=10000*(DJI/com2$DJI[1]), color = "DJI")) +
                 geom_line(aes(x=Date, y=10000*(DOV/com2$DOV[1]), color = "DOV")) +
                 geom_line(aes(x=Date, y=10000*(NYA/com2$NYA[1]), color = "NYA")) +
                 geom_line(aes(x=Date, y=10000*(XAX/com2$XAX[1]), color = "XAX")) +
                 geom_line(aes(x=Date, y=10000*(ROP/com2$ROP[1]), color = "ROP")) +
                 labs(title = "Holding period return", x="Time(daily)", y="Holding period return"))
      
      b <- ggplotly(ggplot(log2)+
                      geom_line(aes(x=Date, y=log2$FWWFX, color="FWWFX")) +
                      geom_line(aes(x=Date, y=log2$MSCI_world, color = "MSCI_world"))+
                      geom_line(aes(x=Date, y=log2$NASDAQ, color = "NASDAQ"))+
                      geom_line(aes(x=Date, y=log2$SP_500, color = "SP_500")) +
                      geom_line(aes(x=Date, y=log2$CWI, color = "CWI")) +
                      geom_line(aes(x=Date, y=log2$DJI, color = "DJI")) +
                      geom_line(aes(x=Date, y=log2$DOV, color = "DOV")) +
                      geom_line(aes(x=Date, y=log2$NYA, color = "NYA")) +
                      geom_line(aes(x=Date, y=log2$XAX, color = "XAX")) +
                      geom_line(aes(x=Date, y=log2$ROP, color = "ROP")) +
                      labs(title = "Daily log return & Holding period return", x="Time(daily)", y="Daily log return"))
      subplot(a,b, nrows = 2, shareX = TRUE)
    }
    else if(input$choose1 == "FIVFX"){
      a1 <- ggplotly(ggplot(com)+
                       geom_line(aes(x=Date, y=10000*(FIVFX/com$FIVFX[1]), color = "FIVFX")) +
                       geom_line(aes(x=Date, y=10000*(EEM/com$EEM[1]), color = "EEM"))+
                       geom_line(aes(x=Date, y=10000*(JFAMX/com$JFAMX[1]), color = "JFAMX"))+
                       geom_line(aes(x=Date, y=10000*(MOAT/com$MOAT[1]), color = "MOAT")) +
                       geom_line(aes(x=Date, y=10000*(GSPC/com$GSPC[1]), color = "GSPC")) +
                       geom_line(aes(x=Date, y=10000*(IXIC/com$IXIC[1]), color = "IXIC")) +
                       geom_line(aes(x=Date, y=10000*(EAFE/com$EAFE[1]), color = "EAFE")) +
                       geom_line(aes(x=Date, y=10000*(MSCIUSA/com$MSCIUSA[1]), color = "MSCIUSA")) +
                       geom_line(aes(x=Date, y=10000*(ASX200/com$ASX200[1]), color = "ASX200")) +
                       geom_line(aes(x=Date, y=10000*(CAC40/com$CAC40[1]), color = "CAC40")) +
                       geom_line(aes(x=Date, y=10000*(FGIRX/com$FGIRX[1]), color = "FGIRX")) +
                       geom_line(aes(x=Date, y=10000*(Hang/com$Hang[1]), color = "Hang")) +
                       geom_line(aes(x=Date, y=10000*(SP/com$SP[1]), color = "SP")) +
                       geom_line(aes(x=Date, y=10000*(N225/com$N225[1]), color = "N225")) +
                       labs(title = "Holding period return", x="Time(daily)", y="Holding period return"))
      b1 <- ggplotly(ggplot(log)+
                       geom_line(aes(x=Date, y=log$FIVFX_r, color = "FIVFX_r")) +
                       geom_line(aes(x=Date, y=log$EEM_r, color = "EEM_r"))+
                       geom_line(aes(x=Date, y=log$JFAMX_r, color = "JFAMX_r"))+
                       geom_line(aes(x=Date, y=log$MOAT_r, color = "MOAT_r")) +
                       geom_line(aes(x=Date, y=log$GSPC_r, color = "GSPC_r")) +
                       geom_line(aes(x=Date, y=log$IXIC_r, color = "IXIC_c")) +
                       geom_line(aes(x=Date, y=log$EAFE_r, color = "EAFE_r")) +
                       geom_line(aes(x=Date, y=log$MSCIUSA_r, color = "MSCIUSA_r")) +
                       geom_line(aes(x=Date, y=log$ASX200_r, color = "ASX200_r")) +
                       geom_line(aes(x=Date, y=log$CAC40_r, color = "CAC40_r")) +
                       geom_line(aes(x=Date, y=log$FGIRX_r, color = "FGIRX_r")) +
                       geom_line(aes(x=Date, y=log$Hang_r, color = "Hang_r")) +
                       geom_line(aes(x=Date, y=log$SP_r, color = "SP_r")) +
                       geom_line(aes(x=Date, y=log$N225_r, color = "N225_r")) +
                       labs(title = "Daily log return & Holding period return", x="Time(daily)", y="Daily log return"))
      subplot(a1,b1, nrows = 2, shareX = TRUE)
    }
    else if(input$choose1 == "FHKCX"){
      a2 <- ggplotly(ggplot(com1)+
                       geom_line(aes(x=Date, y=10000*(FHKCX/com1$FHKCX[1]), color="FHKCX"))+
                       geom_line(aes(x=Date, y=10000*(MSCI/com1$MSCI[1]), color="MSCI")) +
                       geom_line(aes(x=Date, y=10000*(Nikkei/com1$Nikkei[1]), color="Nikkei"))+
                       geom_line(aes(x=Date, y=10000*(Hang/com1$Hang[1]), color="Hang"))+
                       geom_line(aes(x=Date, y=10000*(BSE/com1$BSE[1]), color="BSE"))+
                       geom_line(aes(x=Date,y=10000*(VWO/com1$VWO[1]),color="VWO"))+
                       geom_line(aes(x=Date,y=10000*(IXIC/com1$IXIC[1]),color = "IXIC"))+
                       ylab("holding period return"))
      b2 <- ggplotly(ggplot(log1)+
                       geom_line(aes(x=Date, y=log1$FHKCX_return, color="FHKCX_return"))+
                       geom_line(aes(x=Date, y=log1$MSCI_return, color="MSCI_return"))+
                       geom_line(aes(x=Date, y=log1$Nikkei_return, color="Nikkei_return"))+
                       geom_line(aes(x=Date, y=log1$Hang_return, color="Hang_return"))+
                       geom_line(aes(x=Date, y=log1$BSE_return, color="BSE_return"))+
                       geom_line(aes(x=Date, y=log1$VWO_return,color="VWO_return"))+
                       geom_line(aes(x=Date, y=log1$IXIC_return,color = "IXIC"))+
                       labs(title = "Daily log return & Holding period return"), x="Time(daily)")
      subplot(a2,b2, nrows = 2, shareX = TRUE)
    }
    else if(input$choose1 == "FIVLX"){
      a3 <- ggplotly(ggplot(com3)+ 
                       geom_line(aes(x=Date, y=10000*(FIVLX/com3$FIVLX[1]), color="FIVLX"))+
                       geom_line(aes(x=Date, y=10000*(MSCI_Val/com3$MSCI_Val[1]), color="MSCI_Val"))+
                       geom_line(aes(x=Date, y=10000*(CAC40/com3$CAC40[1]), color="CAC40"))+
                       geom_line(aes(x=Date, y=10000*(FTSE.100/com3$FTSE.100[1]), color="FTSE 100"))+
                       geom_line(aes(x=Date, y=10000*(NIKKEI/com3$NIKKEI[1]), color="NIKKEI"))+
                       geom_line(aes(x=Date, y=10000*(MSCI_Europe/com3$MSCI_Europe[1]), color="MSCI_Europe"))+
                       ylab("holding period return")+
                       theme_bw())
      b3 <- ggplotly(ggplot(log3)+
                       geom_line(aes(x=Date, y=log3$FIVLX_return, color = "FIVLX_return"))+
                       geom_line(aes(x=Date, y=log3$MSCI_Val_return, color="MSCI_Val_return"))+
                       geom_line(aes(x=Date, y=log3$CAC40_return, color="CAC40_return"))+
                       geom_line(aes(x=Date, y=log3$FTSE_return, color="FTSE_return"))+
                       geom_line(aes(x=Date, y=log3$NIKKEI_return, color="NIKKEI_return"))+
                       geom_line(aes(x=Date, y=log3$MSCI_Europe_return, color="MSCI_Europe_return"))+
                       labs(title = "Daily log return & Holding period return", x="Time(daily)", y="Daily log return"))
      subplot(a3,b3, nrows = 2, shareX = TRUE)}
    else if(input$choose1 == "FIGRX"){
      a4 <- ggplotly(ggplot(com4)+
                       geom_line(aes(x=Date, y=10000*(FGIRX/com4$FGIRX[1])))+
                       geom_line(aes(x=Date, y=10000*(MSCI_EAFE/com4$MSCI_EAFE[1]), color="MSCI_EAFE"))+
                       geom_line(aes(x=Date, y=10000*(SPBMI/com4$SPBMI[1]), color="SPBMI"))+
                       geom_line(aes(x=Date, y=10000*(EFAD/com4$EFAD[1]), color="EFAD"))+
                       # geom_line(aes(x=Date, y=10000*(DJII/com4$DJII[1]), color="DJII"))+
                       geom_line(aes(x=Date, y=10000*(VWO/com4$VWO[1]), color="VWO"))+
                       # geom_line(aes(x=Date, y=10000*(hang_seng/com4$hang_seng[1]), color="hang_seng"))+
                       geom_line(aes(x=Date, y=10000*(IMTM/com4$IMTM[1]), color="IMTM"))+
                       geom_line(aes(x=Date, y=10000*(MSCI_euro/com4$MSCI_euro[1]), color="MSCI_euro"))+
                       geom_line(aes(x=Date, y=10000*(MSCI_EAFE_val/com4$MSCI_EAFE_val[1]), color="MSCI_EAFE_val"))+
                       ylab("holding period return"))
      b4 <- ggplotly(ggplot(log4)+
                       geom_line(aes(x=Date, y=log4$FGIRX_return))+
                       geom_line(aes(x=Date, y=log4$MSCI_EAFE_return, color="MSCI_EAFE_return"))+
                       geom_line(aes(x=Date, y=log4$SPBMI_return, color="SPBMI_return"))+
                       geom_line(aes(x=Date, y=log4$EFAD_return, color="EFAD_return"))+
                       geom_line(aes(x=Date, y=log4$IMTM_return, color="IMTM_return"))+
                       geom_line(aes(x=Date, y=log4$VWO_return,color="VWO_return"))+
                       geom_line(aes(x=Date, y=log4$MSCI_euro_return,color = "MSCI_euro_return"))+
                       geom_line(aes(x=Date, y=log4$MSCI_EAFE_val_return,color = "MSCI_EAFE_val_return"))+
                       labs(title = "Daily log return & Holding period return", x="Time(daily)"))
      subplot(a4,b4, nrows = 2, shareX = TRUE)
    }
  })
  
  ##FIVFX
  compositfivfx <- log %>% dplyr::select(-Date,-FIVFX_r,-EAFE_r, -MSCIUSA_r) %>% as.matrix()
  composit_newfivfx <- as.data.frame(compositfivfx) %>% dplyr::select(JFAMX_r,GSPC_r,IXIC_r, EEM_r, MOAT_r) %>% as.matrix()
  One_lasso_newfivfx <- ones(n=nrow(composit_newfivfx),m=1)
  FIVFX_Lasso <- log %>% dplyr::select(FIVFX_r) %>% as.matrix()
  cv_compositfivfx <- cv.glmnet(x=compositfivfx, y=FIVFX_Lasso)
  cv_composit_newfivfx <- cv.glmnet(x=composit_newfivfx,y=FIVFX_Lasso)
  lambdafivfx <- cv_compositfivfx$lambda.min
  lambda_newfivfx <- cv_composit_newfivfx$lambda.min
  lasso_coef_newfivfx <- round(coef(cv_composit_newfivfx,s=lambdafivfx) %>% as.matrix(),1)
  lasso_coef_new_namefivfx <- paste(unlist(rownames(lasso_coef_newfivfx)[-1]),collapse = ",")
  lasso_mat_newfivfx <- cbind(One_lasso_newfivfx,composit_newfivfx)
  lasso_composit_new_indexfivfx <- lasso_mat_newfivfx %*% lasso_coef_newfivfx
  coefsfivfx <- data.frame(lasso_coef_newfivfx[-1])
  namesfivfx <- data.frame(dimnames(lasso_coef_newfivfx)[[1]][-1])
  coefsfivfx <- dplyr::bind_cols(namesfivfx,coefsfivfx)
  colnames(coefsfivfx) <- c("index","coef")
  full_namefivfx <- data.frame(colnames(compositfivfx))
  colnames(full_namefivfx) <- c("index")
  full_coeffivfx <- left_join(full_namefivfx,coefsfivfx,"index")
  full_coeffivfx$coef[is.na(full_coeffivfx$coef)] <- 0.
  ##FWWFX
  composit_FWWFX <- log2 %>% dplyr::select(-Date,-FWWFX,-MSCI_world) %>% as.matrix()
  composit_new_FWWFX <- as.data.frame(composit_FWWFX) %>% dplyr::select(NASDAQ,SP_500,CWI, DJI, DOV,NYA,XAX,ROP) %>% as.matrix()
  One_lasso_newFWWFX <- ones(n=nrow(composit_new_FWWFX),m=1)
  FWWFX_Lasso <- log2 %>% dplyr::select(FWWFX) %>% as.matrix()
  cv_compositFWWFX <- cv.glmnet(x=composit_FWWFX, y=FWWFX_Lasso)
  cv_composit_newFWWFX  <- cv.glmnet(x=composit_new_FWWFX,y=FWWFX_Lasso)
  lambdaFWWFX  <- cv_compositFWWFX$lambda.min
  lambda_newFWWFX <- cv_composit_newFWWFX$lambda.min
  lasso_coef_newFWWFX <- round(coef(cv_composit_newFWWFX,s=lambdaFWWFX) %>% as.matrix(),1)
  lasso_coef_new_nameFWWFX <- paste(unlist(rownames(lasso_coef_newFWWFX)[-1]),collapse = ",")
  lasso_mat_newFWWFX <- cbind(One_lasso_newFWWFX,composit_new_FWWFX)
  lasso_composit_new_indexFWWFX <- lasso_mat_newFWWFX %*% lasso_coef_newFWWFX
  coefsfwwfx <- data.frame(lasso_coef_newFWWFX[-1])
  namesfwwfx <- data.frame(dimnames(lasso_coef_newFWWFX)[[1]][-1])
  coefsfwwfx <- dplyr::bind_cols(namesfwwfx,coefsfwwfx)
  colnames(coefsfwwfx) <- c("index","coef")
  full_namefwwfx <- data.frame(colnames(composit_FWWFX))
  colnames(full_namefwwfx) <- c("index")
  full_coeffwwfx <- left_join(full_namefwwfx,coefsfwwfx,"index")
  full_coeffwwfx$coef[is.na(full_coeffwwfx$coef)] <- 0.
  #FHKCX
  compositfhkcx <- log1  %>% dplyr::select(-c(Date,FHKCX_return,MSCI_return)) %>% as.matrix()
  composit_newfhkcx <- log1  %>% dplyr::select(-c(Date,FHKCX_return,MSCI_return, CWI_return)) %>% as.matrix()
  One_lassofhkcx <- ones(n=nrow(composit_newfhkcx),m=1)
  FHKCX_Lasso <- log1$FHKCX %>% as.matrix()
  cv_compositfhkcx <- cv.glmnet(x=compositfhkcx,y=FHKCX_Lasso)
  lambdafhkcx <- cv_compositfhkcx$lambda.min
  lasso_coeffhkcx <- round(coef(cv_compositfhkcx, s=lambdafhkcx) %>% as.matrix(),1)
  coefsfhkcx <- data.frame(lasso_coeffhkcx[-1])
  namesfhkcx <- data.frame(dimnames(lasso_coeffhkcx)[[1]][-1])
  coefsfhkcx <- dplyr::bind_cols(namesfhkcx,coefsfhkcx)
  colnames(coefsfhkcx) <- c("index","coef")
  full_namefhkcx <- data.frame(colnames(compositfhkcx))
  colnames(full_namefhkcx) <- c("index")
  full_coeffhkcx <- left_join(full_namefhkcx,coefsfhkcx,"index")
  full_coeffhkcx$coef[is.na(full_coeffhkcx$coef)] <- 0.
  #FGIRX
  x = log4  %>% dplyr::select(-c(Date,FGIRX_return,MSCI_EAFE_return))%>%as.matrix()
  y = log4$FGIRX_return %>% as.matrix()
  lambda_seq = 10^seq(10, -10, by = -.1)
  ## Method One, select lambda according to cross validation
  cv_output <- cv.glmnet(x = x, y = y, alpha = 1, lambda = lambda_seq)
  best_lam <- cv_output$lambda.min
  lasso_best <- glmnet(x = x, y = y, alpha = 1, lambda = best_lam)
  pred = predict(lasso_best, s = best_lam, newx = x)
  ssr = sum((pred - y)^2)
  sst = sum((y - mean(y))^2)
  R2 = 1 - ssr/sst
  a = round(coef(lasso_best),1)
  p = length(a@x) - 1
  n = length(y)
  R2.adj = R2 - (1 - R2)*(p/(n - p -1))
  fgirx <- as.matrix(round(coef(lasso_best),1))
  coefsfgirx <- data.frame(fgirx[-1])
  namesfgirx <- data.frame(dimnames(fgirx)[[1]][-1])
  coefsfgirx <- dplyr::bind_cols(namesfgirx,coefsfgirx)
  colnames(coefsfgirx) <- c("index","coef")
  full_namefgirx <- data.frame(colnames(x))
  colnames(full_namefgirx) <- c("index")
  full_coeffgirx <- left_join(full_namefgirx,coefsfgirx,"index")
  full_coeffgirx$coef[is.na(full_coeffgirx$coef)] <- 0.
  #FIVLX 
  x1 = as.matrix(log3[-c(1:3)])
  y1 = as.matrix(log3[2])
  lambda_seq1 = 10^seq(0, -10, by = -.1)
  cv_output1 <- cv.glmnet(x = x1, y = y1, alpha = 1, lambda = lambda_seq1)
  best_lam1 <- cv_output1$lambda.min
  lasso_best1 <- glmnet(x = x1, y = y1, alpha = 1, lambda = best_lam1)
  pred1 = predict(lasso_best1, s = best_lam1, newx = x1)
  ssr1 = sum((pred1 - y1)^2)
  sst1 = sum((y1 - mean(y1))^2)
  R21 = 1 - ssr1/sst1
  a1 = coef(lasso_best1)
  p1 = length(a1@x) - 1
  n1 = length(y1)
  R2.adj1 = R21 - (1 - R21)*(p1/(n1 - p1 -1))
  fivlx <- as.matrix(round(coef(lasso_best1),1))
  coefsfivlx <- data.frame(fivlx[-1])
  namesfivlx <- data.frame(dimnames(fivlx)[[1]][-1])
  coefsfivlx <- dplyr::bind_cols(namesfivlx,coefsfivlx)
  colnames(coefsfivlx) <- c("index","coef")
  full_namefivlx <- data.frame(colnames(x1))
  colnames(full_namefivlx) <- c("index")
  full_coeffivlx <- left_join(full_namefivlx,coefsfivlx,"index")
  full_coeffivlx$coef[is.na(full_coeffivlx$coef)] <- 0.
  output$model <- renderPlotly({
    if(input$choose1 == "FIVLX"){
      ggplotly(ggplot(full_coeffivlx)+aes(x = index,y = coef,fill = index)+
                 geom_col()+
                 geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
                 geom_hline(yintercept = 0,aes(color = "Zero"))+
                 ylab("weights")+
                 ggtitle("Weights Estimated for Composite index")+
                 theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 15))
      )}
    else if(input$choose1 == "FIVFX"){
      ggplotly(ggplot(full_coeffivfx)+aes(x = index,y = coef,fill = index)+
                 geom_col()+
                 geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
                 geom_hline(yintercept = 0,aes(color = "Zero"))+
                 ylab("weights")+
                 ggtitle("Weights Estimated for Composite index")+
                 theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 15)))
    }
    else if(input$choose1 == "FWWFX"){
      ggplotly(ggplot(full_coeffwwfx)+aes(x = index,y = coef,fill = index)+
                 geom_col()+
                 geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
                 geom_hline(yintercept = 0,aes(color = "Zero"))+
                 ylab("weights")+
                 ggtitle("Weights Estimated for Composite index")+
                 theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 15)))
    }
    else if(input$choose1 == "FHKCX"){
      ggplotly(ggplot(full_coeffhkcx)+aes(x = index,y = coef,fill = index)+
                 geom_col()+
                 geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
                 geom_hline(yintercept = 0,aes(color = "Zero"))+
                 ylab("weights")+
                 ggtitle("Weights Estimated for Composite index")+
                 theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 15)))
    }
    else if(input$choose1 == "FIGRX"){
      ggplotly(ggplot(full_coeffgirx)+aes(x = index,y = coef,fill = index)+
                 geom_col()+
                 geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
                 geom_hline(yintercept = 0,aes(color = "Zero"))+
                 ylab("weights")+
                 ggtitle("Weights Estimated for Composite index")+
                 theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 15)))
    }
  })
  
  ##################################### Sector Fund ######################################################
  output$html1 <- renderUI({
    if(input$choose == "FRESX"){
          includeHTML("FRESX.Rhtml")
        }
    else if(input$choose == "FSMEX"){
      includeHTML("FSMEX.Rhtml")
    }
    else if(input$choose == "FSPCX"){
      includeHTML("FSPCX.Rhtml")
    }
    else if(input$choose == "FSPTX"){
      includeHTML("FSPTX.Rhtml")
    }
    else if(input$choose == "FSCPX"){
      includeHTML("FSCPX.Rhtml")
    }
  }) 
  output$Result1 <- renderPlotly({
    if(input$choose == "FRESX"){
      ggplot(Full_weight_FRESX)+aes(x = index,y = weights,fill = index)+
        geom_col()+
        geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
        geom_hline(yintercept = 0,aes(color = "Zero"))+
        ylab("weights")+
        ggtitle("Weights Estimated for Composite index")+
        theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 22))
    }
    else if(input$choose == "FSMEX"){
      ggplot(Full_weight_FSMEX)+aes(x = index,y = weights,fill = index)+
        geom_col()+
        geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
        geom_hline(yintercept = 0,aes(color = "Zero"))+
        ylab("weights")+
        ggtitle("Weights Estimated for Composite index")+
        theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 22))
    }
    else if(input$choose == "FSPCX"){
      ggplot(Full_weight_FSPCX)+aes(x = index,y = weights,fill = index)+
        geom_col()+
        geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
        geom_hline(yintercept = 0,aes(color = "Zero"))+
        ylab("weights")+
        ggtitle("Weights Estimated for Composite index")+
        theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 22))
    }
    else if(input$choose == "FSPTX"){
      ggplot(Full_weight_FSPTX)+aes(x = index,y = weights,fill = index)+
        geom_col()+
        geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
        geom_hline(yintercept = 0,aes(color = "Zero"))+
        ylab("weights")+
        ggtitle("Weights Estimated for Composite index")+
        theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 22))
    }
    else if(input$choose == "FSCPX"){
      ggplot(Full_weight_FSCPX)+aes(x = index,y = weights,fill = index)+
        geom_col()+
        geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
        geom_hline(yintercept = 0,aes(color = "Zero"))+
        ylab("weights")+
        ggtitle("Weights Estimated for Composite index")+
        theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 22))
    }
    else if(input$choose == "Group"){
      ggplot(Full_weight)+aes(x = index,y = weights,fill = sector)+
        geom_col()+
        geom_hline(yintercept = 0,linetype = "dashed",alpha = .5)+
        geom_hline(yintercept = 0,aes(color = "Zero"))+
        ylab("weights")+
        ggtitle("Weights Estimated for Composite index")+
        theme(legend.position = "",axis.text.x = element_text(angle = 35),axis.title.x = element_blank(),plot.title = element_text(face = "bold",size = 22))
    }
    })
    output$Plot1 <- renderPlot({
      if(input$choose == "FRESX"){
        model_test <- glmnet(x = composite1, y = FRESX_Lasso, alpha = input$slidea,lambda = exp(input$slideb))
        coef_plot <- round(coef(model_test)%>%
                             as.matrix(),1)%>%data.frame()%>%tibble::rownames_to_column(var = "fund_ticker")
        ggplot(coef_plot)+
          geom_col(aes(x = fund_ticker,y = s0,fill = fund_ticker),show.legend = FALSE)+
          theme(axis.text.x = element_text(angle = 35))+
          xlab("Indices")+
          ylab("Weights")
      }
      else if(input$choose == "FSMEX"){
        model_test <- glmnet(x = composite2, y = FSMEX_Lasso, alpha = input$slidea,lambda = exp(input$slideb))
        coef_plot <- round(coef(model_test)%>%
                             as.matrix(),1)%>%data.frame()%>%tibble::rownames_to_column(var = "fund_ticker")
        ggplot(coef_plot)+
          geom_col(aes(x = fund_ticker,y = s0,fill = fund_ticker),show.legend = FALSE)+
          theme(axis.text.x = element_text(angle = 35))+
          xlab("Indices")+
          ylab("Weights")
      }
      else if(input$choose == "FSPCX"){
        model_test <- glmnet(x = composite3, y = FSPCX_Lasso, alpha = input$slidea,lambda = exp(input$slideb))
        coef_plot <- round(coef(model_test)%>%
                             as.matrix(),1)%>%data.frame()%>%tibble::rownames_to_column(var = "fund_ticker")
        ggplot(coef_plot)+
          geom_col(aes(x = fund_ticker,y = s0,fill = fund_ticker),show.legend = FALSE)+
          theme(axis.text.x = element_text(angle = 35))+
          xlab("Indices")+
          ylab("Weights")
      }
      # else if (input$choose == "FSPTX"){
      #   model_test <- glmnet(x = composite4, y = FSPTX_Lasso, alpha = input$slidea,lambda = exp(input$slideb))
      #   coef_plot <- round(coef(model_test)%>%
      #                        as.matrix(),1)%>%data.frame()%>%tibble::rownames_to_column(var = "fund_ticker")
      #   ggplot(coef_plot)+
      #     geom_col(aes(x = fund_ticker,y = s0,fill = fund_ticker),show.legend = FALSE)+
      #     theme(axis.text.x = element_text(angle = 35))+
      #     xlab("Indices")+
      #     ylab("Weights")
      # }
})
  #################################### End of Sector Fund############################################
  
  ##################################### Bond Fund ######################################################
    
    
  output$modelfbndx<-renderPlotly({
    
    ggplotly(
      fbn
    )
  })
  
    output$modelfmsfx<-renderPlotly({
      
      ggplotly(
        fms
      )
    })
    
    
    output$modelfmndx<-renderPlotly({
      
      ggplotly(
        fmn
      )
    })
    
    output$modelsphix<-renderPlotly({
      
      ggplotly(
        sph
      )
    })
    output$plot_fcsPlot<-renderPlotly({
      
      ggplotly(
        fcx
      )
    })
  output$Graph.shp <- renderPlotly({
    
    ggplotly(
      sphix
    )
  })
  
  output$Graph.fmn <- renderPlotly({
    
    ggplotly(
      fmndx
    )
  })
  output$Graph.fbn <- renderPlotly({
    
    ggplotly(
      fbndx
    )
  })
  output$Graph.fcs <- renderPlotly({
    
    ggplotly(
     fcstx
    )
  })
  output$Graph.fms <- renderPlotly({
    plot.JYP
  })
  output$FMNDX_Harry<- renderText({
    
    "This investment seeks to provide a high level of income, exempt from federal income tax, consistent with preservation of capital. 
    The fund typically invests at least 80% of its assets in U.S. dollar-denominated municipal money market securities and high quality 
      investment-grade municipal debt securities whose interest is exempt from federal income tax. It invests up to 10% of assets in lower-quality investment-grade securities. 
      The fund typically invests at least 80 percent of its capital in U.S.-dollar municipal money market securities and high-quality municipal debt securities whose value is exempt from federal income tax."
    
    
  })
  output$FBNDX_Yuanyuan<- renderText({
    
    "Fidelity Investment Grade Bond Fund is a U.S.-based open-end fund. The goal of this fund is to achieve a high level of current income. 
The fund typically invests at least 80% of assets in all types of investment-grade debt securities and repurchase agreements for those securities. 
The benchmark of the fund is a Bloomberg Barclays Index."
    
    
  })
  output$FCSTX_Teresa<- renderText({ 
    "This investment requires a high current income level, excluded from personal income taxes from federal and California. 
The fund typically invests at least 80% of assets in investment-grade municipal debt securities whose interest is exempt from individual income taxes from federal and California.
It may spend more than 25 percent of total assets in municipal securities that finance specific types of projects. 
    The fund typically has an average dollar-weighted lifespan of two to five years. It is non-diversified."
    
  })
  output$SPHIX_Fiona<- renderText({ 
    "Established in August 1990 and operated by Fidelity Group, this high-yield bond fund's goal is to pursue a high current profit, 
and it usually invests mainly in income-generating debt securities, preferred stocks, 
and convertible securities, with a focus on debt securities of lower quality. This fund invests in distressed or unpredictable financial circumstances in 
    businesses and domestic and foreign issuers, and it may invest in securities that are not producing income, like default securities and common stocks."
    
  })
  output$FMSFX_Yiping<- renderText({ 
    "Following conservative investment risk, the Fidelity Advisor Mortgage Securities Fund seeks a high level of current income.
The fund may also consider the potential for capital gain in search of current income. 
    For these assets, the fund invests in investment-grade mortgage-related securities and repurchase agreements. 
    The index of the fund is the U.S. MBS Index of Bloomberg Barclays."
  })
  output$test<- renderText({ 
    "ghghghghghgh."
  })
  output$groupImg<- renderImage({
    Leg<-"www/IMG_0816.JPG"
    list(src = Leg)
  },deleteFile = FALSE)
  ###################################End of bond fund#################################
})
