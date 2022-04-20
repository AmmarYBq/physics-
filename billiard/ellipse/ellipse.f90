program elipse
implicit none 
double precision :: y,x,a,b,pi
double precision :: y1,x1,angle,e,c, r
double precision :: v0,vx,vy,t,tmax,dt
double precision :: magn,magt,vNor,vPar, thc
integer :: counter,lines

pi=4.0d0*atan(1.0d0)
a=3d0
b=2.6d0
y1=0d0
t=0d0
tmax=500d0
dt=0.001d0


counter=0d0



open (unit=1 , file="results.txt")
open (unit=2 , file ="position.txt")

!e=sqrt(1-b**2/a**2) ! activate these to adjust ellipse size through a and b (major and minor axes"
!c=sqrt(a**2-b**2)

e=0.6d0            ! disable the above and activate these to adjust ellipse size through e "eccentricity" and a "major axis"
c=a*e
b=sqrt(a**2-c**2)

angle=0d0*pi/180d0
do while (angle<=360d0*pi/180d0)
 counter=counter+1
 r=(a*b)/(sqrt((a**2)*sin(angle)**2+(b**2)*cos(angle)**2))
 x1=r*cos(angle)
 y1=r*sin(angle)
angle=angle+0.1d0*pi/180d0 


write (1,*) x1,y1,c,-c

end do 


x=0.7d0          !position of the ball initially "dont position the ball out side the chosen ellipse"
y=0d0
v0=3d0           ! initial velocity
lines=tmax/dt
counter=0d0
angle=55d0*pi/180d0  ! angle of the ball initally
vx=v0*cos(angle)
vy=v0*sin(angle)

do while(t<tmax)
counter=counter+1



x=x+vx*dt
y=y+vy*dt


if (x**2/a**2+y**2/b**2>=0.98d0*1d0)  call reflection(vx,vy,y,x,a,b)
 
if (mod(counter,lines/6000)==1) write (2,*) x,y,thc
t=t+dt

end do
close (1)
close (2)



end program
subroutine reflection(vx,vy,y,x,a,b)
implicit none

double precision :: vx, vy,thc,a,b 
double precision :: vNor, vPar,y,x,mag

thc=atan((y-0d0)/(x-0d0))  ! thc=atan ((y-yc)/(x-xc))
                           ! where xc,yc represent the center of the ellipse 
						   ! since our ellipse is in the middle xc=0 yc=0
						   
mag=1d0/sqrt((a**2)*sin(thc)**2+(b**2)*cos(thc)**2) !magnitude

vNor=mag*(b*vx*cos(thc)+a*vy*sin(thc))     !"normal and tangent unit vector to an ellipse"
vPar=mag*(-a*vx*sin(thc)+b*vy*cos(thc))

vx=-vNor*cos(thc)-vPar*sin(thc)            !" transform back to x,y cordinates with respect to normal and tanget " 
vy=-vNor*sin(thc)+vPar*cos(thc)            ! (write V components "vx,vy" using Vtangent, Vnormal)


end subroutine reflection