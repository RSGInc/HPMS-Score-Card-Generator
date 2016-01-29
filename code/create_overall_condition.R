###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# Author needs to add a description!
#
###########################################################################

create_overall_condition <- function(data,state,year,population)
{
     # the algorithm is described in detail in the file title MAP-21 Pavement Condition Rule
  
     # four metrics are used based on the surface type
     rutting  <- data[state_code==state&year_record==year&data_item=="RUTTING"&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric),]
     setnames(rutting,"value_numeric","rutting")
     iri      <- data[state_code==state&year_record==year&data_item=="IRI"&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric),]
     setnames(iri,"value_numeric","iri")
     faulting <- data[state_code==state&year_record==year&data_item=="FAULTING"&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric),]
     setnames(faulting,"value_numeric","faulting")
     cracking <- data[state_code==state&year_record==year&data_item=="CRACKING_PERCENT"&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric),]
     setnames(cracking,"value_numeric","cracking")
     
     surface <- data[state_code==state&year_record==year&data_item=="SURFACE_TYPE"&FACILITY_TYPE!=4,list(route_id,F_SYSTEM,Interstate,NHS,begin_point,end_point,value_numeric),]
     setnames(surface,"value_numeric","surface")
     
     urban <- data[state_code==state&year_record==year&data_item=="URBAN_CODE"&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric),]
     setnames(urban,"value_numeric","urban_code")
     
     if(nrow(rutting)==0|nrow(iri)==0|nrow(faulting)==0|nrow(cracking)==0|nrow(surface)==0|nrow(rutting)==0)
     {
       # exit if not enough data to complete the analysis 
       return(textGrob(NoDataString,gp=gpar(fontsize=12, col="Red")))
     } else
     {
       urban[,rural:=0]
       urban[urban_code==99999,rural:=1]
       urban[!is.na(urban_code)&is.na(rural),rural:=0]
       
       iri <- sqldf("select A.*,B.urban_code,B.rural from iri A left join urban B on A.route_id = B.route_id and A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point")
       iri <- data.table(iri)
       
       iri <- merge(iri,population,by="urban_code",all.x=TRUE,all.y=FALSE)
       
       iri[rural==1,threshold:=170]
       iri[(rural==0)*(pop<1000000),threshold:=170]
       iri[(rural==0)*(pop>=1000000),threshold:=220]
       
       rutting[(rutting<0.20)               ,rscore:= "G"] # good 
       rutting[(rutting>=0.20&rutting<=0.40),rscore:= "F"] # fair
       rutting[(rutting>0.40)               ,rscore:= "P"] # poor
       
       faulting[(faulting< 0.05)               ,fscore:= "G"] # good
       faulting[(faulting>=0.05&faulting<=0.15),fscore:= "F"] # fair
       faulting[(faulting> 0.15)               ,fscore:= "P"] # poor
       
       cracking[(cracking<5)              ,cscore:= "G"] # good
       cracking[(cracking>=5&cracking<=10),cscore:= "F"] # fair
       cracking[(cracking>10)             ,cscore:= "P"] # poor
  
       # iri is a function of population size and urban code
       
       iri[(iri< 95)                         ,iscore:= "G"] # good 
       iri[(iri>=95)       & (iri<=threshold),iscore:= "F"] # fair
       iri[(iri> threshold)                  ,iscore:= "P"] # poor
       
       #condition <- merge(surface,  rutting, by=c("route_id","begin_point","end_point"),all.x=TRUE,all.y=FALSE)
       #condition <- merge(condition,faulting,by=c("route_id","begin_point","end_point"),all.x=TRUE,all.y=FALSE)
       #condition <- merge(condition,cracking,by=c("route_id","begin_point","end_point"),all.x=TRUE,all.y=FALSE)
       #condition <- merge(condition,iri,     by=c("route_id","begin_point","end_point"),all.x=TRUE,all.y=FALSE)
       
       condition <- sqldf("select A.*, B.cracking, B.cscore
                          from iri A 
                          left join cracking B on 
                            A.route_id = B.route_id and (
                            ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                            ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                          )")
       
       condition <- sqldf("select A.*, B.faulting, B.fscore
                          from condition A 
                         left join faulting B on 
                          A.route_id = B.route_id and (
                         ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                         ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                     )")
       
       condition <- sqldf("select A.*, B.rutting, B.rscore
                          from condition A 
                          left join rutting B on 
                            A.route_id = B.route_id and (
                            ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                            ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                          )")
       
       condition <- sqldf("select A.*, B.surface,B.Interstate,B.NHS
                          from condition A 
                          left join surface B on 
                            A.route_id = B.route_id and (
                            ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                            ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                          )")
       
      condition <- data.table(condition)
       
       #table 4.5 in the manual is very useful
       
       # rutting: 2,6,7,8
       # faulting: 3,4,9,10
       # CRCP: 5
       
       
       # asphalt 
       condition[surface %in% c(2,6,7,8) &!is.na(rscore)&!is.na(cscore)&!is.na(iscore),oscore:=paste0(rscore,cscore,iscore)]

       # jointed concrete
       # added surface type 11 (other) as concrete to match Max Grogg's process
       condition[surface %in% c(3,4,9,10,11)&!is.na(fscore)&!is.na(cscore)&!is.na(iscore),oscore:=paste0(fscore,cscore,iscore)]

       # Continuously reinforced concrete
       condition[surface %in% c(5)       &               !is.na(cscore)&!is.na(iscore),oscore:=paste0(       cscore,iscore)]
       
       countChar <- function(x,stringtofind)
       {
            return(length(regmatches(x,gregexpr(stringtofind,x))[[1]]))
       }
       
       condition[nchar(oscore)==3,overallscore:=1*(countChar(oscore,"G")==3)+3*(countChar(oscore,"P")>=2),by=.(route_id,begin_point,end_point)]
       condition[!is.na(oscore)&overallscore==0&nchar(oscore)==3,overallscore:=2]
       condition[nchar(oscore)==2,overallscore:=1*(countChar(oscore,"G")==2)+3*(countChar(oscore,"P")==2),by=.(route_id,begin_point,end_point)]
       condition[!is.na(oscore)&overallscore==0&nchar(oscore)==2,overallscore:=2]
       
       condition[,overallscore:=c(NA,"G","F","P")[1+overallscore]]    
       
       #results.interstate <- table(condition[Interstate==1,factor(overallscore,levels=c("G","F","P"))])
       #results.nhs        <- table(condition[NHS==1,       factor(overallscore,levels=c("G","F","P"))])
       #results.fsystem1   <- table(condition[F_SYSTEM==1,  factor(overallscore,levels=c("G","F","P"))])
       #results.fsystem2   <- table(condition[F_SYSTEM==2,  factor(overallscore,levels=c("G","F","P"))])
       
       results.interstate <- condition[Interstate==1,list(.N,miles=sum(end_point-begin_point)), by=.(overallscore)]
       results.NHS        <- condition[NHS==1,list(.N,miles=sum(end_point-begin_point)), by=.(overallscore)]
       
       if(nrow(results.interstate)>0)
       {
            #p1 <- create_donut_chart(results.interstate)
            p1 <- barOCPlot(results.interstate,"Interstate")
       } else
       {
            p1 <- textGrob(NoDataString,gp=gpar(fontsize=12, col="Red"))
       }
       
       if(nrow(results.NHS)>0)
       {
            #p2 <- create_donut_chart(results.nhs)
            p2 <- barOCPlot(results.NHS,"NHS")
       } else
       {
            p2 <- textGrob(NoDataString,gp=gpar(fontsize=12, col="Red"))
       }
       
       obj <- arrangeGrob(
                textGrob("The algorithm applies the MAP-21\ncondition rule to classify\nsections as good, fair or poor.",gp=gpar(col="slategray",fontface="bold",fontsize=9,hjust=0)),
                p1,p2,ncol=3,nrow=1,widths=unit(c(0.2,0.4,0.4),units="npc"))
       
       #obj <- arrangeGrob(
      #                     # row 1
      #                     rectGrob(gp=gpar(fill="white",col="white")), 
      #                     rectGrob(gp=gpar(fill="white",col="white")), 
      #                     rectGrob(gp=gpar(fill="white",col="white")), 
      #                     rectGrob(gp=gpar(fill="white",col="white")), 
      #                     rectGrob(gp=gpar(fill="white",col="white")), 
                           
                           # row 2
      #                     rectGrob(gp=gpar(fill="white",col="white")), 
      #                     textGrob("Interstate",gp=gpar(col="slategray",fontface="bold",fontsize=9)),
      #                     textGrob("National Highway System",gp=gpar(col="slategray",fontface="bold",fontsize=9)),
      #                     textGrob("Principal Arterial Systems",gp=gpar(col="slategray",fontface="bold",fontsize=9)),
      #                     textGrob("Lower Level Systems",gp=gpar(col="slategray",fontface="bold",fontsize=9)),
            
                           # row 3
                           # labels
      #                     arrangeGrob(rectGrob(gp=gpar(fill="white",col="white"))          ,rectGrob(gp=gpar(fill="white",col="white")),
      #                                 textGrob("Good",hjust=1,gp=gpar(col="slategray")),            rectGrob(,gp=gpar(fill="gray65",col="gray65")),
      #                                 rectGrob(gp=gpar(fill="white",col="white"))          ,rectGrob(gp=gpar(fill="white",col="white")),
      #                                 textGrob("Fair",hjust=1,gp=gpar(col="slategray")),            rectGrob(gp=gpar(fill="gray85",col="gray85"))        ,
      #                                 rectGrob(gp=gpar(fill="white",col="white"))          ,rectGrob(gp=gpar(fill="white",col="white")),
      #                                 textGrob("Poor",hjust=1,gp=gpar(col="slategray")),            rectGrob(gp=gpar(fill="red",col="red"))          ,
      #                                 rectGrob(gp=gpar(fill="white",col="white"))          ,rectGrob(gp=gpar(fill="white",col="white")),
      #                                 ncol=2,nrow=7,widths=unit(c(0.8,0.2),units="npc"),heights=c(0.1,0.7/3,0.05,0.7/3,0.05,0.7/3,0.1)
      #                     ),
      #                     # plots
      #                    p1,p2,p1,p2,
      #                    ncol=5,nrow=3,
      #                    heights=c(0.05,0.1,0.85),
      #                    widths=unit(c(0.15,rep(0.85/4,4)),units="npc"))
       
       return(obj)
     }
     
}