### A piper diagram based on the ternary plot example here: http://srmulcahy.github.io/2012/12/04/ternary-plots-r.html
### This was written quickly, and most likely contains bugs - I advise you to check it first.
### Jason Lessels jlessels@gmail.com

### This now consists of two functions. transform_piper_data transforms the data to match 
### the coordinates of the piper diagram. ggplot_piper does all of the background.


transform_piper_data <- function(Mg, Ca, Cl,SO4, name=NULL){
  if(is.null(name)){
    name = rep(1:length(Mg),3)
  } else {
    name = rep(name,3)
  }
  y1 <- Mg * 0.86603
  x1 <- 100*(1-(Ca/100) - (Mg/200))
  y2 <- SO4 * 0.86603
  x2 <-120+(100*Cl/100 + 0.5 * 100*SO4/100)
  new_point <- function(x1, x2, y1, y2, grad=1.73206){
    b1 <- y1-(grad*x1)
    b2 <- y2-(-grad*x2)
    M <- matrix(c(grad, -grad, -1,-1), ncol=2)
    intercepts <- as.matrix(c(b1,b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x=t_mat[1,1], y=t_mat[2,1])
  }
  np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))
  npoints <- do.call("rbind",np_list)
  data.frame(observation=name,lbl=rep(c('cat','an','piper'), each=length(x1)),x=c(x1, x2, npoints$x), y=c(y=y1, y2, npoints$y))
}


ggplot_piper <- function() {
  library(ggplot2)
  grid1p1 <<- data.frame(x1 = c(20,40,60,80), x2= c(10,20,30,40),y1 = c(0,0,0,0), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid1p2 <<- data.frame(x1 = c(20,40,60,80), x2= c(60,70,80,90),y1 = c(0,0,0,0), y2 = c(69.2824, 51.9618,34.6412,17.3206))
  grid1p3 <<- data.frame(x1 = c(10,20,30,40), x2= c(90,80,70,60),y1 = c(17.3206,34.6412,51.9618, 69.2824), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid2p1 <<- grid1p1
  grid2p1$x1 <- grid2p1$x1+120
  grid2p1$x2 <- grid2p1$x2+120
  grid2p2 <<- grid1p2
  grid2p2$x1 <- grid2p2$x1+120
  grid2p2$x2 <- grid2p2$x2+120
  grid2p3 <<- grid1p3
  grid2p3$x1 <- grid2p3$x1+120
  grid2p3$x2 <- grid2p3$x2+120
  grid3p1 <<- data.frame(x1=c(100,90, 80, 70),y1=c(34.6412, 51.9618, 69.2824, 86.603), x2=c(150, 140, 130, 120), y2=c(121.2442,138.5648,155.8854,173.2060))
  grid3p2 <<- data.frame(x1=c(70, 80, 90, 100),y1=c(121.2442,138.5648,155.8854,173.2060), x2=c(120, 130, 140, 150), y2=c(34.6412, 51.9618, 69.2824, 86.603))
  
  p <- ggplot() +
    ## left hand ternary plot
    geom_segment(aes(x=0,y=0, xend=100, yend=0)) +
    geom_segment(aes(x=0,y=0, xend=50, yend=86.603)) +
    geom_segment(aes(x=50,y=86.603, xend=100, yend=0)) +
    ## right hand ternary plot
    geom_segment(aes(x=120,y=0, xend=220, yend=0)) +
    geom_segment(aes(x=120,y=0, xend=170, yend=86.603)) +
    geom_segment(aes(x=170,y=86.603, xend=220, yend=0)) +
    ## Upper diamond
    geom_segment(aes(x=110,y=190.5266, xend=60, yend=103.9236)) +
    geom_segment(aes(x=110,y=190.5266, xend=160, yend=103.9236)) +
    geom_segment(aes(x=110,y=17.3206, xend=160, yend=103.9236)) +
    geom_segment(aes(x=110,y=17.3206, xend=60, yend=103.9236)) +
    ## Add grid lines to the plots
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    ### Labels and grid values
    #geom_text(aes(50,-10, label="Ca^2"), parse=T, size=4) + # Commented out, as parse=TRUE can cause issues
    
    geom_text(aes(c(20,40,60,80),c(-5,-5,-5,-5), label=c(80, 60, 40, 20)), size=3) +
    geom_text(aes(c(35,25,15,5),grid1p2$y2, label=c(80, 60, 40, 20)), size=3) +
    geom_text(aes(c(95,85,75,65),grid1p3$y2, label=c(80, 60, 40, 20)), size=3) +
    # geom_text(aes(17,50, label="Mg^2"), parse=T, angle=60, size=4) +
    coord_equal(ratio=1)+  
    geom_text(aes(17,50, label="Mg^{2+phantom()}"), angle=60, size=4, parse=TRUE) +  
    geom_text(aes(82.5,50, label="Na^+phantom()~+~K^+phantom()"), angle=-60, size=4,parse=TRUE) +
    geom_text(aes(50,-10, label="Ca^{2+phantom()}"), size=4, parse=TRUE) +
    
    
    geom_text(aes(170,-10, label="Cl^-phantom()"), size=4, parse=TRUE) +
    geom_text(aes(205,50, label="SO[4]^{2-phantom()}"), angle=-60, size=4, parse=TRUE) +
    geom_text(aes(137.5,50, label="HCO[3]^{-phantom()}"), angle=60, size=4, parse=TRUE) +
    geom_text(aes(72.5,150, label="SO[4]^{2-phantom()}~+~Cl^-phantom()"), angle=60, size=4, parse=TRUE) +
    geom_text(aes(147.5,150, label="Ca^{2+phantom()}~+~Mg^{2+phantom()}"), angle=-60, size=4, parse=TRUE) + 
    
    geom_text(aes(c(155,145,135,125),grid2p2$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(c(215,205,195,185),grid2p3$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(c(140,160,180,200),c(-5,-5,-5,-5), label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p1$x1-5,grid3p1$y1, label=c(80, 60, 40, 20)), size=3) +
    geom_text(aes(grid3p1$x2+5,grid3p1$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p2$x1-5,grid3p2$y1, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p2$x2+5,grid3p2$y2, label=c(80, 60, 40, 20)), size=3) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank())
  return(p)
}


ggplot_biplot <- function(X, labels, x=1, y=2, col = NULL, alpha=1, size=2, trans = NULL) {
  s = svd(scale(X, scale=F, center=F))
  obs = data.frame(s$u %*% diag(s$d^alpha))
  names(obs) = paste0('Comp.', 1:ncol(obs))
  
  vars = data.frame(t(diag(s$d^(1-alpha) ) %*% t(s$v)))
  names(vars) = names(obs)
  vars$varnames = labels
  
  x_lab = sprintf('Comp.%d',x)
  y_lab = sprintf('Comp.%d',y)
 
  if(is.null(col)){
    obs$col = 1
  }else{
    obs$col = col
  }
  plot = ggplot() + geom_point(data=obs, aes_string(x=x_lab, y=y_lab, color='col'),
                               size=size, alpha=alpha)
  if(is.null(col)){
    plot = ggplot() + geom_point(data=obs, aes_string(x=x_lab, y=y_lab),
                                 color='blue', size=size, alpha=alpha)
  }
  mxy_obs = max( abs(max(obs[,c(x_lab,y_lab)])), abs(min(obs[,c(x_lab,y_lab)])))
  mxy_vars = max( abs(max(vars[,c(x_lab,y_lab)])), abs(min(vars[,c(x_lab,y_lab)])))
  
  vars = vars %>% dplyr::mutate(
    v1 = .55 *  mxy_obs / mxy_vars * vars[,x_lab],
    v2 = .55 *  mxy_obs / mxy_vars * vars[,y_lab])
  
  
  pca = summary(prcomp(X))$importance
  
  plot + geom_segment(data=vars, aes(x=0, y=0, xend=v1, yend=v2, text = varnames), 
                      arrow=arrow(length=unit(0.2,"cm")), 
                      alpha=0.75, size=1.3, col='red', alpha=0.5) +
    geom_text(data=vars, aes(label=varnames, x=v1, y=v2), size=5) + theme_bw() + 
    xlab(sprintf('Prin.Comp.%d (%5.2f %%)', x, pca[2,x]*100)) + 
    ylab(sprintf('Prin.Comp.%d (%5.2f %%)', y, pca[2,y]*100))
}