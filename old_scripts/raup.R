make_elliptic_generating_shape <- function(D,S){
  #Let's define the original ray as 1
  a = 1
  rc = (D+1)*a/(1-D)
  t<-seq(0,2*pi,by=pi/100)
  b = a/S
  circle_0 = cbind(r=rc + a*cos(t), y= b*sin(t), phi=0)
  return(circle_0)
}

coiling <- function(RT,W,generating_shape, turns,resolution){
  PHI = seq(0,2*pi*turns,length=resolution)
  far_end = generating_shape[1,1]
  closest_end = approx(generating_shape[generating_shape[,1]<far_end-1,2],
         generating_shape[generating_shape[,1]<far_end-1,1],0)$y
  D = closest_end/far_end
  rc = (D+1)/(1-D)
  rho = function(theta, W, r0) r0 * W^(theta/(2*pi))
  y = function(y0,W,theta,rc,T) y0 * W^(theta/(2*pi)) + rc*T*(W^(theta/(2*pi))-1)
  circle = apply(generating_shape,1,
                 function(x)lapply(PHI,
                                   function(theta)cbind(r=rho(theta, W, x['r']),
                                                        y=y(x['y'],W,theta, rc,RT),
                                                        phi=theta)))
  circle = do.call(rbind,lapply(circle,function(x)do.call(rbind,x)))
  
  #To cartesian coordinates
  XYZ = list(X = circle[,1] * cos(circle[,3]),
             Y = circle[,1] * sin(circle[,3]),
             Z = circle[,2])
  XYZ
}

coil_elliptic <- function(rate_of_translation, distance_to_axis, whorl_expansion, shape, turns=1, steps=1000){
  circle <- make_elliptic_generating_shape(distance_to_axis,shape)
  coiling(rate_of_translation,whorl_expansion,circle,turns,steps)
}

dots2polygon <- function(XYZ,steps){
  obj <- as.data.frame(XYZ)
  n_gen <- nrow(obj)/steps
  spl <- split(obj,rep(1:n_gen,each=steps))
  emb <- embed(c(1:n_gen,1),2)
  POLY <- do.call(rbind,apply(emb,1,function(x)rbind(spl[[x[1]]],spl[[x[2]]][n_gen:1,],c(NA,NA,NA))))
}
