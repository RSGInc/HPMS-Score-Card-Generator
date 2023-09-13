###################################################################
# expands data set to 0.01 mile increments for easier joining
expand = function(data,increment){
  
  if (nrow(data) == 0) return(data[, num_sections := vector(mode = 'numeric', length = 0)])
  
  data[,beginpoint:=round(beginpoint+ 0.00001,nchar(1/increment)-1)] # forcing rounding to next highest integer
  data[,  end_point:=round(end_point  + 0.00001,nchar(1/increment)-1)]
  
  # dividing up the num sections so we can recover the 
  # num sections later
  data[,num_sections:=1/((end_point-beginpoint)/increment)]
  
  # how long do we have to make the faux road network
  range = data[,.(start=min(beginpoint),end=max(end_point))]
  
  route_network = data.table(end = range[,start]:(range[,end]*(1/increment))/(1/increment))
  route_network[,start:=end-increment]
  
  route_network[,start:=round(start + 0.00001,nchar(1/increment)-1)] # rounding here is necessary to guarantee correct results when looking at inequalities
  route_network[,  end:=round(end   + 0.00001,nchar(1/increment)-1)]
  
  route_network = route_network[start >= 0]
  
  data.expanded = data[route_network,
                       on=.(beginpoint <= start, end_point >= end),
                       allow.cartesian=TRUE]
  
  data.expanded = data.expanded[!is.na(routeid),]
  
  return(data.expanded)
}
