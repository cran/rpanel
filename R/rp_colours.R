#     Set standard colours to be used by other rpanel functions

rp.colours <- function(cols) {
   clrs <- c(estimate  = '#B3CDE3', estline = '#0093FF',
             reference = '#FBB4AE', refline = '#FF7F00',
             points    = 'grey50',  notch   = 'white',
             density   = 'grey75',  bgdcol  = 'grey85',
             node      = 'grey85')
   if (!missing(cols)) {
      if (!is.character(cols)) {
         warning('cols must be a character variable. Default colours will be used.')
      }
      else {
         ncols <- length(cols)
         ind   <- match(names(cols), names(clrs))
         cols  <- cols[!is.na(ind)]
         if (length(cols) < ncols) warning('some elements of cols have mismatched names.')
         if (length(cols) > 0) clrs[names(cols)] <- cols
      }
   }
   clrs
}

rp.colors <- rp.colours
