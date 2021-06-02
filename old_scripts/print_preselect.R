library(shiny)
library(shinyWidgets)
library(mwshiny)
library(rgl)
library(shinyjs)

setwd("~/Git/raup_model") #Set the working directory containing the script and the picture folder
addResourcePath(prefix = "static", directoryPath = "static")

#Function to make the aperture shape
make_elliptic_generating_shape <- function(D,S,res=100){
  #Let's define the original ray as 1
  a <- 1
  rc <- (D+1)*a/(1-D)
  t<-seq(0,2*pi,by=pi/res)
  b <- a/S
  circle_0 <- cbind(r=rc + a*cos(t), y= b*sin(t), phi=0)
  return(circle_0)
}

#Function to coil the shape around the axis
coiling <- function(RT,W,generating_shape, turns,steps,dir="dextral"){
  PHI <- seq(0,2*pi*turns,length=steps)
  far_end <- generating_shape[1,1]
  closest_end <- approx(generating_shape[generating_shape[,1]<far_end-1,2],
                        generating_shape[generating_shape[,1]<far_end-1,1],0)$y
  D <- closest_end/far_end
  rc <- (D+1)/(1-D)
  rho <- function(theta, W, r0) r0 * W^(theta/(2*pi))
  y <- function(y0,W,theta,rc,T) y0 * W^(theta/(2*pi)) + rc*T*(W^(theta/(2*pi))-1)
  circle <- apply(generating_shape,1,
                  function(x)lapply(PHI,
                                    function(theta)cbind(r=rho(theta, W, x['r']),
                                                         y=y(x['y'],W,theta, rc,RT),
                                                         phi=theta)))
  circle <- do.call(rbind,lapply(circle,function(x)do.call(rbind,x)))
  
  #To cartesian coordinates
  if(dir=="dextral"){
    XYZ <- list(X = circle[,1] * sin(circle[,3]),
                Y = circle[,1] * cos(circle[,3]),
                Z = circle[,2])
  }else{
    XYZ <- list(X = circle[,1] * cos(circle[,3]),
                Y = circle[,1] * sin(circle[,3]),
                Z = circle[,2])
  }
  XYZ
}

#Function to transform the final shape into polygons
pt2quad <- function(pts,steps,res){
  pts$pt_n<- rep(1:(res*2+1),each=steps)
  pts$step <- 1:steps
  eg <- expand.grid(1:(2*res),1:(steps-1))
  apply(eg,1,function(x)c(which(pts$step==x[2] & pts$pt_n==x[1]),
                          which(pts$step==x[2]+1 & pts$pt_n==x[1]),
                          which(pts$step==x[2]+1 & pts$pt_n==x[1]+1),
                          which(pts$step==x[2] & pts$pt_n==x[1]+1)))
}

process_preselect <- function(RT,D,W,S,turns,name){
  rgl.open()
  res <- 20
  steps <- 20*turns
  circle <- make_elliptic_generating_shape(D,S,res)
  ce <- coiling(RT,W,circle,turns,steps,"dextral")
  CE <- as.data.frame(ce)
  CE <- CE[c(2,3,1)]
  CE[,2] <- -1*CE[,2]
  qd <- pt2quad(CE,steps,res)
  mqd <- mesh3d(CE,quads=qd)
  rgl.viewpoint(zoom = 1, theta=0, phi=0)
  bg3d(color="transparent")
  material3d(color="grey50",emission="black",alpha=1,specular="white",ambient="black",textype="luminance")
  shade3d(mqd,override=TRUE)
#  light3d(theta=10,phi=10)
  snapshot3d(sprintf("%s_front.png",name),height=1500,width=1500)
  rgl.viewpoint(zoom = 1, theta=0, phi=-90)
  snapshot3d(sprintf("%s_top.png",name),height=1500,width=1500)
  rgl.close()
}

presel <- read.table("~/Git/raup_model/old_scripts/instructions/silhouettes.csv",header=TRUE,sep="\t")
for(i in 1:2){
  process_preselect(presel$RT[i],presel$D[i],presel$W[i],presel$S[i],
                    presel$turns[i],name=presel$Group[i])
  cat(presel$Group[i],"\r")
}
cat("\n")
