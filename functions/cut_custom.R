###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# A custom cut function. Used in the histograms on the summary pages.
#
###########################################################################

cut_custom <- function(x)
{
     bin <- 1 + 
          1 * (x > -100)+
          1 * (x > -75) +
          1 * (x > -50) +
          1 * (x > -25) +
          1 * (x > -15) +
          1 * (x > -5) +
          1 * (x > -1) +
          1 * (x >  -1e-3) +
          1 * (x >   1e-3) +
          1 * (x >  1) +
          1 * (x >  5) +
          1 * (x >  15) +
          1 * (x >  25) +
          1 * (x >  50) +
          1 * (x >  75) +
          1 * (x > 100)       
     
    
     bin[is.na(bin)] <- 18   
     
     
     return(factor(bin,levels=1:18,
                   labels=c("< -100%",
                            "-100%",
                            "-75%",
                            "-50%",
                            "-25%",
                            "-15%",
                            "-5%",
                            "-1%",
                            "0%",
                            "1%",
                            "5%",
                            "15%",
                            "25%",
                            "50%",
                            "75%",
                            "100%",
                            "> 100%",
                            "No Match"
                   )))
}