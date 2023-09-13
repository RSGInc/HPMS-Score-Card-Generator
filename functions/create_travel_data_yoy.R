###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Create travel data year over year change rates. Looking for same change
# rate. Produces a single histogram.
#
# Create travel data year over year change rates
# looking for the same change rate
# this produces a single histogram
#
###########################################################################

create_travel_data_yoy <- function(
  data, 
  state,
  year,
  yearcomparison,
  variable,
  histtype,
  fontsize=6,
  ramps
){
  
  col_same = gColors$dark
  col_changed = gColors$light
  col_not_matched = gColors$text
  col_highlight = gColors$highlight
  
  
  type <- gVariables[Name == variable, Data_Type]
  
  if(type == 'numeric'){
    if(ramps){
      var.1 = data[stateid == state & datayear == year &
                     dataitem == variable & FACILITY_TYPE == 4,
                   list(routeid, beginpoint, endpoint,
                        value.1 = valuenumeric, F_SYSTEM)]
      
      var.2 = data[stateid == state & datayear == yearcomparison &
                     dataitem == variable & FACILITY_TYPE == 4,
                   list(routeid,beginpoint,endpoint,
                        value.2 = valuenumeric, F_SYSTEM)]
    } else {
      var.1 = data[stateid == state & datayear == year &
                     dataitem == variable & FACILITY_TYPE != 4,
                   list(routeid,beginpoint,endpoint,
                        value.1 = valuenumeric, F_SYSTEM)]
      
      var.2 = data[stateid == state & datayear == yearcomparison &
                     dataitem == variable & FACILITY_TYPE != 4,
                   list(routeid,beginpoint,endpoint,
                        value.2 = valuenumeric, F_SYSTEM)]       
    }   
  }
  
  if(type == 'date'){
    if(ramps){
      var.1 = data[stateid == state&datayear == year &
                     dataitem == variable & FACILITY_TYPE == 4 & !is.na(value_date),
                   list(routeid,beginpoint,endpoint,
                        value.1 = year(value_date),F_SYSTEM)]
      
      var.2 = data[stateid == state&datayear == yearcomparison &
                     dataitem == variable & FACILITY_TYPE == 4 & !is.na(value_date),
                   list(routeid,beginpoint,endpoint,
                        value.2 = year(value_date),F_SYSTEM)]
      
    } else {
      var.1 = data[stateid == state & datayear == year &
                     dataitem == variable & FACILITY_TYPE != 4 & !is.na(value_date),
                   list(routeid,beginpoint,endpoint,
                        value.1 = year(value_date),F_SYSTEM)]
      
      var.2 = data[stateid == state & datayear == yearcomparison &
                     dataitem == variable & FACILITY_TYPE != 4 & !is.na(value_date),
                   list(routeid,beginpoint,endpoint,
                        value.2 = year(value_date),F_SYSTEM)]       
    }
  }
  
  if(type == 'text'){
    if(ramps){
      var.1 = data[stateid == state & datayear == year &
                     dataitem == variable & FACILITY_TYPE == 4 & !is.na(valuetext),
                   list(routeid,beginpoint,endpoint,value.1 = valuetext,F_SYSTEM)]
      
      var.2 = data[stateid == state&datayear == yearcomparison &
                     dataitem == variable & FACILITY_TYPE == 4 & !is.na(valuetext),
                   list(routeid,beginpoint,endpoint,value.2 = valuetext,F_SYSTEM)]
      
    } else {
      var.1 = data[stateid == state & datayear == year &
                     dataitem == variable & FACILITY_TYPE != 4 & !is.na(valuetext),
                   list(routeid,beginpoint,endpoint, value.1 = valuetext,F_SYSTEM)]
      
      var.2 = data[stateid == state & datayear == yearcomparison &
                     dataitem == variable & FACILITY_TYPE != 4 & !is.na(valuetext),
                   list(routeid,beginpoint,endpoint,value.2 = valuetext,F_SYSTEM)]       
    }
  }

  if ( yearcomparison <= 2020 ){
    expectedChange <- gVariables[Name == variable, Expect_YOY_change_2020]
  } else {
    expectedChange <- gVariables[Name == variable, Expect_YOY_change]
  }
  
  if(expectedChange == "Y"){
    ff1 <- "*"
    ff2 <- ""
  } else {
    ff2 <- "*"
    ff1 <- ""
  }
  
  # this does the merge at the most disaggregate level
  # keep mismatches of yr1 but not yr2
  var.yoy = merge(var.1, var.2, by = c('routeid', 'beginpoint', 'endpoint'),
                  all.x=TRUE, all.y=FALSE)
  
  if(nrow(var.yoy) > 0){ # we have something to report
    
    if(histtype == 1){
      report = var.yoy[, change := NA_real_]
      report[value.1 == 0 & value.2 == 0, change := 0]
      report[value.2 != 0, change := (value.1 - value.2) / value.2]
      report[, change := change * 100]
      
      # need to use geom_bar and construct a custom histogram
      report[, bin2 := cut_custom(change)] # using custom function to have more control
      totalmiles <- report[, sum(endpoint - beginpoint)]
      report <- report[, endpoint := endpoint / totalmiles]
      report <- report[, beginpoint := beginpoint / totalmiles]
      report[change < -1e-3, color := factor("Reduction")]
      report[change >= -1e-3 & change <= 1e3, color := factor("Same")]
      report[change > 1e-3, color := factor("Increase")]
      report[is.na(change), color := factor("No Match")]
      
    } else {
      report <- var.yoy[, bin2 := factor(1 + 1 * (value.1 != value.2), 
                                         levels=c(1,2), 
                                         labels=c("No Change","Changed"))]
      report[as.numeric(bin2) == 1, color := "No"]
      report[as.numeric(bin2) == 2, color := "Yes"]
      report[is.na(bin2), color := "NA"]
      report$color <- factor(report$color, levels=c('NA', 'Yes', 'No'))
      report <- report[, .(V1 = sum(endpoint - beginpoint)), by=.(color)]
      totalmiles <- report[, sum(V1)]
      report <- report[, V1 := V1 / totalmiles]
      report[, color2 := color]
      report <- merge(data.table(color2 = factor(c("No", "Yes", "NA"))), report,
                      by="color2", all.x=TRUE)
      
      report[is.na(color), V1 := 0]
      report[is.na(color), color := color2]
      
    }
    
    # custom axis labels
    if(histtype == 1){
      p <- ggplot(report, aes(x=bin2,fill=color,weight=endpoint-beginpoint)) +
        geom_bar(width=0.75)
      
      p <- p + scale_x_discrete(
        "",
        breaks=factor(
          c(1:17,18),
          levels=c(1:17,18),
          labels=c("< -100%","-100%","-75%","-50%","-25%","-15%","-5%","-1%","0%","1%","5%","15%","25%","50%","75%","100%","> 100%","No Match"),
          exclude=NULL),
        drop=FALSE)
      
      p <- p + scale_fill_manual(
        "",
        values=c(
          "Same"=col_same,
          "Reduction"=col_changed,
          "Increase"=col_changed,
          "No Match"=col_not_matched))
      
      p <- p + scale_y_continuous(labels=percent,limits=c(-0.01, 1.05)) 
      
    } else {
      
      p <- ggplot(report, aes(x=1,y=V1,fill=color)) +
        geom_bar(stat="identity",width=0.75) 
      
      p <- p + coord_flip()   
      
      colors <- c(col_same,col_changed,col_not_matched)
      names(colors)[1]<-toString(report[color2 == "No",color])
      names(colors)[2]<-toString(report[color2 == "Yes", color])
      names(colors)[3]<-toString(report[color2 == "NA", color])
      #names(colors)<-levels(report[,color])
      
      p <- p + scale_fill_manual("",values=colors)
      p <- p + scale_y_continuous(labels=percent)
      p <- p + scale_x_continuous(labels=percent)
      p <- p + theme(
        axis.text.x=element_text(angle = 0, hjust = 1, vjust=-0.5, size=5, color=col_same))
    }
    
    p <- p + theme(axis.line=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.background=element_blank(),
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())
    
    if(histtype == 1){
      p <- p + theme(
        axis.text.y = element_text(hjust = 1, size=fontsize),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=fontsize),
        plot.margin = unit(c(top=1, right=0.5, bottom=0.5, left=0.5), units='lines'))
      
      p <- arrangeGrob(
        suppressWarnings(arrangeGrob(p)),
        arrangeGrob(
          textGrob(
            paste0("Total of ",string_format(totalmiles)," centerline miles."),
            hjust=0.5,
            vjust=0,
            gp=gpar(fontsize=4.5, col=col_changed))),
        
        nrow=2,
        heights=unit(c(0.90, 0.1),units="npc"),
        widths=unit(1, units="npc"))
      
    } else {
      p <- p + theme(
        axis.text.y     = element_blank(),
        axis.ticks      = element_blank(),
        legend.position = "none",
        legend.text     = element_blank(),
        plot.margin = unit(c(top=1, right=0.5, bottom=0.5, left=0.5), units='lines'))
      
      p <- arrangeGrob(
        textGrob(""),
        suppressWarnings(arrangeGrob(p)),
        
        arrangeGrob(
          
          textGrob(
            paste0(report[color2 == "No", paste0(round(V1,3)*100,"%",ff1)]),
            hjust=0.5,
            vjust=0 ,
            gp=gpar(fontsize=17, col=col_same,fontface="bold")),
          
          textGrob(
            paste0(report[color2 == "Yes",paste0(round(V1,3)*100,"%",ff2)]),
            hjust=0.5,
            vjust=0,
            gp=gpar(fontsize=17, col=col_changed,fontface="bold")),
          
          textGrob(
            paste0(report[color2 == "NA",paste0(round(V1,3)*100,"%")]),
            hjust=0.5,
            vjust=0,
            gp=gpar(fontsize=17, col=col_not_matched,fontface="bold")),
          
          ncol=3,widths=unit(c(0.4,0.2,0.4),units="npc")),
        
        arrangeGrob(
          
          textGrob(
            "of all miles\nstayed the same",
            hjust=0.5,
            vjust=0 ,
            gp=gpar(fontsize=5.5, col=col_same,fontface="bold")),
          
          textGrob(
            "of all miles\nchanged",
            hjust=0.5,
            vjust=0,
            gp=gpar(fontsize=5.5, col=col_changed,fontface="bold")),
          
          textGrob(
            "of all miles\nwere not matched",
            hjust=0.5,
            vjust=0,
            gp=gpar(fontsize=5.5, col=col_not_matched,fontface="bold")),
          
          ncol=3,widths=unit(c(0.4,0.2,0.4),units="npc")),
        
        arrangeGrob(
          textGrob(
            paste0("Total of ", string_format(totalmiles)," centerline miles."),
            hjust=0.5, 
            vjust=0, 
            gp=gpar(fontsize=5, col=col_changed))),
        
        nrow=5,
        heights=unit(c(0.05, 0.4, 0.22, 0.23, 0.1),units="npc"),
        widths=unit(1,units="npc")
      )
      
    }
    
    return(p)
  } else
  {
    # nothing to report because data are missing
    return(textGrob(NoDataString,gp=gpar(fontsize=8, col=col_highlight)))
  }
  
}