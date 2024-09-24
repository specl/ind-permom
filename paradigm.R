require(shape)
xc=seq(.1,.86,length=7)
yc=seq(.89,.2,length=7)
xhalf=.10
yhalf=.07


blank=function() {
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,typ='n',axes=F)}


screen=function(place,txt=list()){
  rect(place[1]-xhalf,place[2]-yhalf,place[1]+xhalf,place[2]+yhalf,col='antiquewhite')
  if (!is.null(txt$right)) 
    text(place[1]+1.1*xhalf,place[2]+yhalf,txt$right,adj=c(0,1))
  if (!is.null(txt$left)) 
    text(place[1]-1.1*xhalf,place[2]-yhalf,txt$left,adj=c(1,0))
}

dotster=function(place,a, highlight=NULL,...){
  i=0:8
  x=place[1]+((i%%3)-1)*xhalf/2
  y=place[2]+((i%/%3)-1)*yhalf/2
  if (!is.null(highlight)) plotcircle(
    mid=c(x[highlight],y[highlight]),
    r=.02,col='white')
  points(x[a],y[a],pch=19,...)}

masking=function(){
  
  blank()
  screen(c(xc[1],yc[1]),txt=list('right'="727 ms"))
  text(xc[1],yc[1],"+",adj=c(.5,.5))
  screen(c(xc[2],yc[2]),txt=list('right'="364 ms"))
  screen(place=c(xc[3],yc[3]),
       txt=list('right'="6.06 ms",
                'left'="Target"))
  a=c(0,0,0,0,0,0,0,1,0)==1
  dotster(place=c(xc[3],yc[3]),a,cex=.7)
  screen(c(xc[4],yc[4]),
       txt=list('right'="Staircase",
                'left'="ISI"))
  screen(place=c(xc[5],yc[5]),
       txt=list('right'="121 ms",
                'left'="Mask"))
  dotster(place=c(xc[5],yc[5]),rep(T,9),cex=.7)
  screen(place=c(xc[6],yc[6]),txt=list('right'="364 ms"))
  screen(place=c(xc[7],yc[7]))
  dotster(place=c(xc[7],yc[7]),rep(T,9),cex=.5,
        col='red',highlight=8)

    text(.5,1,adj=c(.5,0),"A. Masking Task",cex=1.2)
}



intgr=function(){
  
  blank()
  screen(c(xc[1],yc[1]),txt=list('right'="727 ms"))
  text(xc[1],yc[1],"+",adj=c(.5,.5))
  screen(c(xc[2],yc[2]),txt=list('right'="364 ms"))
  screen(place=c(xc[3],yc[3]),
         txt=list('right'="30.3 ms",
                  'left'="Target I"))
  a=c(0,1,0,1,0,1,0,0,1)==1
  dotster(place=c(xc[3],yc[3]),a,cex=.7)
  screen(c(xc[4],yc[4]),
         txt=list('right'="Staircase",
                  'left'="ISI"))
  screen(place=c(xc[5],yc[5]),
         txt=list('right'="30.3 ms",
                  'left'="Target II"))
  a=c(1,0,1,0,1,0,1,0,0)==1
  dotster(place=c(xc[5],yc[5]),a,cex=.7)
  screen(place=c(xc[6],yc[6]),txt=list('right'="364 ms"))
  screen(place=c(xc[7],yc[7]))
  dotster(place=c(xc[7],yc[7]),rep(T,9),cex=.5,
          col='red',highlight=8)
  
  text(.5,1,adj=c(.5,0),"B. Fusion Task",cex=1.2)
}