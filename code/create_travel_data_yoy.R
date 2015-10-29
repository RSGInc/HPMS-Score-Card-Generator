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
     
     if(ramps)
     {
        var.1    <- data[state_code==state&year_record==year          &data_item==variable&FACILITY_TYPE==4,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]
        var.2    <- data[state_code==state&year_record==yearcomparison&data_item==variable&FACILITY_TYPE==4,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]
     } else {
        var.1    <- data[state_code==state&year_record==year          &data_item==variable&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]
        var.2    <- data[state_code==state&year_record==yearcomparison&data_item==variable&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]       
     }   
     
     expectedChange <- gVariables[Name==variable,YOY_Change]
  
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
     var.yoy <- var.yoy[!is.na(value_numeric.x)&!is.na(value_numeric.y),]
                        
     if(nrow(var.yoy)>0) # we have something to report
     {
          
          if(histtype==1)
          {
               report <- var.yoy[,change:=value_numeric.x/value_numeric.y]
               report[,change:=(change-1)*100]
               
               # need to use geom_bar and construct a custom histogram
               report[, bin2 := cut_custom(change)] # using custom function to have more control
               
               report[change< -1e-3 ,color:=factor("Reduction")]
               report[change>=-1e-3&change<=1e3,color:=factor("Same")]
               report[change>1e-3 ,color:=factor("Increase")]
               report[is.na(change) ,color:=factor("NA")]
               
          } else
          {
               report <- var.yoy[,bin2:=factor(1+1*(value_numeric.x!=value_numeric.y),levels=c(1,2),labels=c("No Change","Changed"))]
               report[as.numeric(bin2)==1,color:=factor("No")]
               report[as.numeric(bin2)==2,color:=factor("Yes")]
               report[is.na(bin2) ,color:=factor("NA")]
          }
          
          # custom axis labels
          p <- ggplot(report, aes(x=bin2,fill=color,weight=end_point.x-begin_point.x)) + geom_bar(width=0.5,stat="bin")
          
          if(histtype==1)
          {
               p <- p + scale_x_discrete("", breaks=factor(c(1:17,NA),levels=c(1:17,NA),labels=c("< -100%","-100%","-75%","-50%","-25%","-15%","-5%","-1%","0%","1%","5%","15%","25%","50%","75%","100%","> 100%","N/A"),exclude=NULL), drop=FALSE)
          } else
          {
               p <- p + scale_x_discrete("", breaks=factor(c(1:2,NA),levels=c(1:2,NA),labels=c("No Change","Changed","NA"),exclude=NULL), drop=FALSE)
          }
          
          
          if(histtype==1)
          {
              if(expectedChange=="Y")
              {
                p <- p + scale_fill_manual("",values=c("Same"="red","Reduction"="gray50","Increase"="gray50","NA"="white"))
              } else
              {
                p <- p + scale_fill_manual("",values=c("Same"="gray50","Reduction"="red","Increase"="red","NA"="white"))
              }
               
               #p <- p + geom_text("+",x=0.8,y=0,colour="slategray")
               #p <- p + geom_text("-",x=0.4,y=0,colour="slategray")
          } else
          {
              if(expectedChange=="Y")
              {
                p <- p + scale_fill_manual("",values=c("Yes"="gray50","No"="red","NA"="white"))
              } else
              {
                p <- p + scale_fill_manual("",values=c("Yes"="red","No"="gray50","NA"="white"))
              } 
          }
          
          p <- p + scale_y_continuous()
          
          p <- p + theme(axis.line=element_blank(),
                         #axis.text.x=element_blank(),
                         axis.text.y=element_text(hjust = 1,size=fontsize),
                         #axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.x = element_text(angle = 90, hjust = 1,size=fontsize),
                         legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank())
          
          #p <- vertically_align(p)
          
          return(p)
     } else
     {
          # nothing to report because data are missing
          return(textGrob(NoDataString,gp=gpar(fontsize=12, col="Red")))
     }
     
}