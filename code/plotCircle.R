plotCircle <- function(data,year,year_compare,variable,startx,starty,C,R)
{
  
  # defaults to low quality
  type <- 1
  
  #get recipe
  recipe <- gVariables[Name==variable,Quality_Recipe]
  
  if(nrow(data[year_record==year&data_item==variable,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)])==0)
  {
    type <- 0  
  } else {
  
    # Recipe 1: if more than ?% of miles contain "outlying" values, identical adjacent values, 
    # or identical year over year values,  data item gets 0 points. Else, it gets 1 point. 
    # [note that we sum the percentages from each] - specific to pavement data items now but 
    # could be expanded if we can fill in the existing data elements spreadsheet.
    if( recipe == 1 )
    {
      outliers <- getOutliers(data,year,variable)

      adjaceny <- getAdjaceny(data,year,variable)
      
      yoy      <- getYOY(data,year,year_compare,variable)
      
      tol_High <- gVariables[Name==variable,Quality_Tolerance_High]
      tol_Med  <- gVariables[Name==variable,Quality_Tolerance_Med] 
      
      outliers[,type:=type + 1 * ( perc_miles < tol_Med ) + 1 * ( perc_miles < tol_High )]
      adjaceny[,type:=type + 1 * ( perc_miles < tol_Med ) + 1 * ( perc_miles < tol_High )]
           yoy[,type:=type + 1 * ( perc_miles < tol_Med ) + 1 * ( perc_miles < tol_High )]
      
      weights <- c(1,1,1,1)
           
      type <- (( outliers[groupCat==1,type] +
        adjaceny[groupCat==1,type] +
             yoy[groupCat==1,type] ) * weights[1] +
      ( outliers[groupCat==2,type] +
        adjaceny[groupCat==2,type] +
             yoy[groupCat==2,type] ) * weights[2] +
      ( outliers[groupCat==3,type] +
        adjaceny[groupCat==3,type] +
             yoy[groupCat==3,type] ) * weights[3] +  
      ( outliers[groupCat==4,type] +
        adjaceny[groupCat==4,type] +
             yoy[groupCat==4,type] ) * weights[4])/16
           
      type <- floor(type)
      
    }
    # Recipe 2: if more than ?% of miles change in value when we expect NO CHANGE or more than 
    # ?% of sections are 'unmatched', data item gets 0 points. Else, it gets 1 point.  
    if( recipe == 2 )
    {
      var.1    <- data[year_record==year          &data_item==variable,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]
      var.2    <- data[year_record==year_compare&data_item==variable,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM)]       
      
      expectedChange <- gVariables[Name==variable,YOY_Change]
      tol_Med  <- gVariables[Name==variable,Quality_Tolerance_Med]
      tol_High <- gVariables[Name==variable,Quality_Tolerance_High]
      
      var.yoy <- sqldf("select 
                          A.route_id,A.F_SYSTEM,A.begin_point as [begin_point.x],A.end_point as [end_point.x],A.value_numeric as [value_numeric.x],B.value_numeric as [value_numeric.y]
                        from [var.1] A 
                        left join [var.2] B on A.route_id = B.route_id and 
                          (
                            (
                              ( A.begin_point <= B.end_point   ) and ( A.end_point   >= B.begin_point ) and 
                              ( A.begin_point >= B.begin_point ) and ( A.end_point   <= B.end_point   )
                            ) or
                            (
                              ( B.begin_point <= A.end_point   ) and ( B.end_point   >= A.begin_point ) and 
                              ( B.begin_point >= A.begin_point ) and ( B.end_point   <= A.end_point   )
                            )
                          ) 
                        ")
       
       var.yoy <- data.table(var.yoy)
       
       report <- var.yoy[,bin2:=factor(1+1*(value_numeric.x!=value_numeric.y),levels=c(1,2),labels=c("No Change","Changed"))]
       report <- report[,sum(end_point.x-begin_point.x),by=.(bin2)]
       totalmiles <- report[,sum(V1)]
       report <- report[,V1:=V1/totalmiles]
       
       if(expectedChange=="Y")
       {
         quality.value <- report[as.numeric(bin2)==1|is.na(bin2),sum(V1)]*100
       }
       if(expectedChange=="N")
       {
         quality.value <- report[as.numeric(bin2)==2|is.na(bin2),sum(V1)]*100
       }
       type <- type + 1 * ( quality.value < tol_Med ) + 1 * ( quality.value < tol_High )
    }
  }
  
  # good quality
  if(type==3){
    grid.circle(x=startx+(C-1)*0.15+0.01+0.011,y=starty-(R-1)*0.020,r=unit(0.007,"npc"),gp=gpar(fill="slategray",col="slategray"))
  }
  # medium quality
  if(type==2){
    grid.circle(x=startx+(C-1)*0.15+0.01+0.011,y=starty-(R-1)*0.020,r=unit(0.007,"npc"),gp=gpar(fill="gray75",col="slategray"))
  }
  # lowquality
  if(type==1){
    grid.circle(x=startx+(C-1)*0.15+0.01+0.011,y=starty-(R-1)*0.020,r=unit(0.007,"npc"),gp=gpar(fill="white",col="slategray"))
  }
  
  if(type==0){
    type <- 1
  }
  
  return(type)
  
}