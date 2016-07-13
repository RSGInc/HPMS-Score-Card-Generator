###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# A comma formatter since the 'percent' function in the 'scales' package
# is bugged.
#
###########################################################################

string_format <- function(x)
{
     if(is.numeric(x))
     {
          x <- round(x,2)
     }
     return(format(x, big.mark=",",scientific=FALSE))
}