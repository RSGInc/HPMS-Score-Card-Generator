###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# Create travel data year over year change rates. Looking for same change
# rate. Produces a single histogram.
#
###########################################################################

#######################################################################
# Create travel data year over year change rates
# looking for the same change rate
# this produces a single histogram
#######################################################################
create_travel_data_yoy <- function(
    data, 
    state,
    year,
    yearcomparison,
    variable,
    histtype,
    fontsize=6,
    ramps
)
{
#    To help with the debugging     
#    year<<-year
#    yearcomparison<<-yearcomparison
#    variable<<-variable
#    state<<-state
#    histtype<<-histtype
#    data<<-data
  
  
     if(ramps)
     {
        var.1    <- data[state_code==state&year_record==year          &data_item==variable&FACILITY_TYPE==4,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]
        var.2    <- data[state_code==state&year_record==yearcomparison&data_item==variable&FACILITY_TYPE==4,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]
     } else {
        var.1    <- data[state_code==state&year_record==year          &data_item==variable&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]
        var.2    <- data[state_code==state&year_record==yearcomparison&data_item==variable&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]       
     }   
     
     expectedChange <- gVariables[Name==variable,YOY_Change]
  
     if(expectedChange=="Y")
     {
       ff1<-"*"
       ff2<-""
     } else {
       ff2<-"*"
       ff1<-""
     }
     
      #var.yoy <- merge(var.1,var.2,by=c("route_id","begin_point","end_point"),all.x=TRUE, all.y=FALSE)
     
     # this does the merge at the most disaggregate level
     #var.yoy <- merge(var.1,var.2,by=c("route_id"),all.x=TRUE, all.y=FALSE,allow.cartesian=T)
     #var.yoy <- var.yoy[((begin_point.x<=end_point.y)&(end_point.x>=begin_point.y)&(begin_point.x>=begin_point.y)&(end_point.x<=end_point.y))|
     #                    ((begin_point.y<=end_point.x)&(end_point.y>=begin_point.x)&(begin_point.y>=begin_point.x)&(end_point.y<=end_point.x)),]
     
     var.yoy <- sqldf("
                      select 
                      A.route_id,
                      A.F_SYSTEM,
                      A.begin_point as [begin_point.x],
                      A.end_point   as [end_point.x],
                      A.value_numeric as [value_numeric.x],
                      B.value_numeric as [value_numeric.y]
                      from [var.1] A 
                      left join [var.2] B on 
                      A.route_id = B.route_id and 
                      (
                      (
                      ( A.begin_point <= B.end_point   ) and
                      ( A.end_point   >= B.begin_point ) and 
                      ( A.begin_point >= B.begin_point ) and 
                      ( A.end_point   <= B.end_point   )
                      ) or
                      (
                      ( B.begin_point <= A.end_point   ) and
                      ( B.end_point   >= A.begin_point ) and 
                      ( B.begin_point >= A.begin_point ) and 
                      ( B.end_point   <= A.end_point   )
                      )
                      ) 
                      ")
     
     var.yoy <- data.table(var.yoy)
     
     # removing the NAs
     #var.yoy <- var.yoy[!is.na(value_numeric.x)&!is.na(value_numeric.y),]
                        
     if(nrow(var.yoy)>0) # we have something to report
     {
          
          if(histtype==1)
          {
               report <- var.yoy[,change:=value_numeric.x/value_numeric.y]
               report[,change:=(change-1)*100]
               
               # need to use geom_bar and construct a custom histogram
               report[, bin2 := cut_custom(change)] # using custom function to have more control
               totalmiles<-report[,sum(end_point.x-begin_point.x)]
               #report <- report[,V1:=sum(end_point.x-begin_point.x)]
               report <- report[,end_point.x:=end_point.x/totalmiles]
               report <- report[,begin_point.x:=begin_point.x/totalmiles]
               report[change< -1e-3 ,color:=factor("Reduction")]
               report[change>=-1e-3&change<=1e3,color:=factor("Same")]
               report[change>1e-3 ,color:=factor("Increase")]
               report[is.na(change) ,color:=factor("No Match")]
               
          } else
          {
               report <- var.yoy[,bin2:=factor(1+1*(value_numeric.x!=value_numeric.y),levels=c(1,2),labels=c("No Change","Changed"))]
               report[as.numeric(bin2)==1,color:=factor("No")]
               report[as.numeric(bin2)==2,color:=factor("Yes")]
               report[is.na(bin2)        ,color:=factor("NA")]
               report <- report[,sum(end_point.x-begin_point.x),by=.(color)]
               totalmiles <- report[,sum(V1)]
               report <- report[,V1:=V1/totalmiles]
               report[,color2:=color]
               report <- merge(data.table(color2=factor(c("No","Yes","NA"))),report,by="color2",all.x=TRUE)
               
               report[is.na(color),V1:=0]
               report[is.na(color),color:=color2]
               #report[,color:=factor(color,labels=paste0(round(V1,2)*100,"%"))]
               
               
          }
          
          # custom axis labels
          
          if(histtype==1)
          {
               p <- ggplot(report, aes(x=bin2,fill=color,weight=end_point.x-begin_point.x)) + geom_bar(width=0.75,stat="bin")
               p <- p + scale_x_discrete("", breaks=factor(c(1:17,18),levels=c(1:17,18),labels=c("< -100%","-100%","-75%","-50%","-25%","-15%","-5%","-1%","0%","1%","5%","15%","25%","50%","75%","100%","> 100%","No Match"),exclude=NULL), drop=FALSE)
               p <- p + scale_fill_manual("",values=c("Same"="slategray","Reduction"="gray50","Increase"="gray50","No Match"="black"))
               p <- p + scale_y_continuous(labels=percent,limits=c(0,1)) 

          } else
          {
              p <- ggplot(report[c(2,3,1),], aes(x=1,y=V1,fill=color)) + geom_bar(stat="identity",width=0.75) 
              #p <- p + scale_x_discrete("", breaks=factor(c(1:2,NA),levels=c(1:2,NA),labels=c("No Change","Changed","NA"),exclude=NULL), drop=FALSE)
              p <- p + coord_flip()   
              colors <- c("slategray","gray50","black")
              names(colors)[1]<-toString(report[color2=="No",color])
              names(colors)[2]<-toString(report[color2=="Yes", color])
              names(colors)[3]<-toString(report[color2=="NA", color])
              #names(colors)<-levels(report[,color])
              p <- p + scale_fill_manual("",values=colors)
              p <- p + scale_y_continuous(labels=percent)
              p <- p + scale_x_continuous(labels=percent)
              p <- p + theme(axis.text.x=element_text(angle = 0, hjust = 1,vjust=-0.5,size=5,color="slategray"))
          }
          
          p <- p + theme(axis.line=element_blank(),
                         #axis.text.x=element_blank(),
                         #axis.text.y=element_text(hjust = 1,size=fontsize),
                         #axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         #axis.text.x = element_text(angle = 90, hjust = 1,size=fontsize),
                         #legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank())
          
          if(histtype==1)
          {
            p <- p + theme(
                          axis.text.y = element_text(hjust = 1,size=fontsize),
                          legend.position = "none",
                          axis.text.x = element_text(angle = 90, vjust = 0.5,size=fontsize))
            p <- arrangeGrob(
                              arrangeGrob(p),
                              arrangeGrob(textGrob(paste0("Total of ",string_format(totalmiles)," centerline miles."),hjust=0.5 ,vjust=0  ,gp=gpar(fontsize=4.5, col="gray50"))),
                              nrow=2,
                              heights=unit(c(0.90,0.1),units="npc"),
                              widths=unit(1,units="npc")
                            )
            
          } else 
          {
             p <- p + theme(
                          axis.text.y     = element_blank(),
                          #axis.text.x     = element_blank(),
                          axis.ticks      = element_blank(),
                          legend.position = "none",
                          legend.text     = element_blank(),
                          plot.margin = unit(c(topMargin=0.1,leftMargin=0.1,bottomMargin=0.1,rightMargin=0.1), "cm"))
                          #plot.margin=unit(c(0.5,0.05,0.5,0.05),"cm"))
             
             p <- arrangeGrob(textGrob(""),
                              arrangeGrob(p),
                              arrangeGrob(textGrob(paste0(report[color2=="No",paste0(round(V1,3)*100,"%",ff1)]),hjust=0.5   ,vjust=0  ,gp=gpar(fontsize=17, col="slategray",fontface="bold")),
                                          textGrob(paste0(report[color2=="Yes",paste0(round(V1,3)*100,"%",ff2)]),hjust=0.5,vjust=0  ,gp=gpar(fontsize=17, col="gray50",fontface="bold")),
                                          textGrob(paste0(report[color2=="NA",paste0(round(V1,3)*100,"%")]),hjust=0.5   ,vjust=0  ,gp=gpar(fontsize=17, col="black",fontface="bold")),
                                       ncol=3,widths=unit(c(0.4,0.2,0.4),units="npc")),
                              arrangeGrob(textGrob("of all miles\nstayed the same" ,hjust=0.5   ,vjust=0  ,gp=gpar(fontsize=5.5, col="slategray",fontface="bold")),
                                          textGrob("of all miles\nchanged"         ,hjust=0.5 ,vjust=0  ,gp=gpar(fontsize=5.5, col="gray50",fontface="bold")),
                                          textGrob("of all miles\nwere not matched",hjust=0.5   ,vjust=0  ,gp=gpar(fontsize=5.5, col="black",fontface="bold")),
                                       ncol=3,widths=unit(c(0.4,0.2,0.4),units="npc")),
                              arrangeGrob(textGrob(paste0("Total of ",string_format(totalmiles)," centerline miles."),hjust=0.5 ,vjust=0  ,gp=gpar(fontsize=5, col="gray50"))),
                              nrow=5,
                              heights=unit(c(0.05,0.35,0.27,0.23,0.1),units="npc"),
                              widths=unit(1,units="npc")
                              )
             
                  
             
            }
          
          #p <- vertically_align(p)
          
          return(p)
     } else
     {
          # nothing to report because data are missing
          return(textGrob(NoDataString,gp=gpar(fontsize=8, col="Red")))
     }
     
}