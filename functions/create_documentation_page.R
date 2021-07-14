# Create documentation page



create_documentation_pages = function(state, year, color='white'){

  
  # Score page
  
  grid.arrange(
    # header
    rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
    
    rectGrob(gp = gpar(fill = "slategray", col = "white")), 
    
    # this text gets overlayed by Kevin's image
    textGrob("information goes here!",gp = gpar(fill = "white", col = "red")), 
    
    nrow=3,heights = unit(c(0.6,0.03,7.5-0.63),units="inches"))
  
  add_header(state, year, "how the scores are calculated","?")
  
  grid.raster(
    image=gScorePage,
    x = 0.01,
    y=0.9,
    width=unit(0.98, 'npc'),
    hjust = 0,
    vjust=1)
  
  gPageNumber <<- gPageNumber + 1
  add_page_number(gPageNumber)
  
  
  # Intpreting graphs page
  
  grid.arrange(
    # header
    rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
    
    rectGrob(gp = gpar(fill = "slategray", col = "white")), 
    
    # this text gets overlayed by Kevin's image
    textGrob("information goes here!",gp = gpar(fill = "white", col = "red")), 
    
    nrow=3,heights = unit(c(0.6,0.03,7.5-0.63),units="inches"))
  
  add_header(state,year,"how to interpret key scorecard charts","?")
  
  grid.raster(image=gInfoPage,x = 0, y=0.9,hjust = 0,vjust=1)
  
  gPageNumber <<- gPageNumber + 1
  add_page_number(gPageNumber)
  
}
