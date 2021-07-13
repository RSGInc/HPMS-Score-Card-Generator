# Create documentation page


create_documentation_page = function(){
  
  grid.arrange(
    rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank))
  )
  
  gPageNumber <<- gPageNumber + 1
  add_page_number(gPageNumber)

}