ggCaterpillar <- function(re, QQ = FALSE, likeDotplot = TRUE, detailedFacetLabs = TRUE) {
  
  #'@title Plot random effects distributions.
  #'@author Stephen Pierzchajlo
  #'@description Plots the distribution of random effects terms from lmer models.
  #'@usage ggCaterpillar(re)
  #'@param re The random effects to be plotted. Can reference a model (ex. ranef(re)[1]).

  f <- function(x, nm = "ranef plot") {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each = nrow(x))
    pDf  <- data.frame(y = unlist(x)[ord],
                       ci = 1.96*se[ord],
                       nQQ = rep(stats::qnorm(stats::ppoints(nrow(x))), ncol(x)),
                       ID = factor(rep(rownames(x), ncol(x))[ord], levels = rownames(x)[ord]),
                       ind = gl(ncol(x), nrow(x), labels = names(x)))
    
    if(detailedFacetLabs){
      pDf$ind <- ifelse(grepl("(Intercept)", pDf$ind), "intercept adjustment",
                        paste0("slope adj: ", pDf$ind))
    }
    
    ## normal QQ-plot
    if(QQ) {  
      p <- ggplot(pDf, aes_string(x = "nQQ", y = "y"))
      p <- p + facet_wrap(~ind, scales = "free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
      
    ## caterpillar dotplot
    } else {  
      p <- ggplot(pDf, aes_string(x = "ID", y = "y")) + coord_flip()
      
      ## imitate dotplot() -> same scales for random effects
      if(likeDotplot) {  
        p <- p + facet_wrap(~ind)
      
      ## different scales for random effects    
      } else {           
        p <- p + facet_grid(ind ~ ., scales = "free_y")
      }
      p <- p + xlab(nm) + ylab("Random effects")
      scale <- 12-log(length(levels(pDf$ID)), 2)
      p <- p + theme(axis.text.y = element_text(size = scale))
    }
    p <- p + theme(legend.position = "none")
    p <- p + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(grDevices::hsv(0/12, 7/12, 7/12)),
                        alpha = I(5/12))
    p <- p + geom_errorbar(aes_string(ymin = "y - ci", ymax = "y + ci"), width = 0, colour = "black")
    p <- p + geom_point(aes())
    return(p)
  }
  #   lapply(re, f) # original
  lapply(seq_along(re), function(y, n, i) { f(y[[i]], n[[i]]) }, y = re, n = names(re))
  }