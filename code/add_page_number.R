add_page_number <- function(number)
{
  grid.text(paste0("page ",number),x=0.995,y=0.015,gp=gpar(fontsize=7, col="slategray"),hjust=1)
}