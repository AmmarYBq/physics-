program pendulum 

implicit none 
double precision :: der_v1,der_th1,v1, pi,g,l1,degth1,t,th1,dt
double precision :: tmax,E1,K1,U1,q1,fd1,wd1,y1,x1,m1
double precision ::der_v2,der_th2,v2,l2,degth2,th2,m2,a,b,c,d
double precision ::E2,K2,U2,q2,fd2,wd2,y2,x2, Etot
double precision :: k1v1, k2v1,k3v1,k4v1,k1v2,k2v2,k3v2,k4v2
integer :: counter , counter2, lines

pi=4.0d0*atan(1.0d0)   
g=9.8d0
l1=10d0    
m1=5d0    
th1=25*pi/180d0      
t=0d0
dt=0.001d0
tmax=250d0
v1=0d0       
K1=0d0
U1=0d0

q1=0.0d0     !damping and driving force
fd1=0.0d0
wd1=0d0

!second pendelum parameters----------------------
l2=5d0   
m2=2d0   
th2=0d0*pi/180d0
v2=0d0 
K2=0d0
U2=0d0

q2=0.0d0    !damping and driving force
fd2=0.0d0
wd2=0d0


lines=(tmax/dt)

 


counter=0
counter2=0
! obtain v by rk4

open(unit=1, file="results1.txt")
open (unit=2, file="resultsPo1.txt")
open(unit=3, file="results2.txt")
open (unit=4, file="resultsPo2.txt")
do while (t<=tmax)
counter=counter+1

der_th1=v1
der_th2=v2

th1=th1+der_th1*dt
th2=th2+der_th2*dt

a=(m2*(l2**2)*(der_th2**2)*sin(th1-th2))+((m1+m2)*l2*g*sin(th1))
b=(m2*l1*l2*(der_th1**2)*sin(th1-th2)*cos(th1-th2))-(m2*l2*g*sin(th2)*cos(th1-th2))
c=-((m1+m2)*l1*l2)+(m2*l1*l2*(cos(th1-th2)**2))
d=a+b

k1v1=(d/c )-(q1)*(der_th1)+fd1*sin(wd1*t)
k1v2=(((l1*(der_th1**2)*sin(th1-th2))-(l1*(k1v1)*cos(th1-th2))-(g*sin(th2)))/l2)!-(q1)*(der_th1)+fd1*sin(wd1*t)

der_th1=der_th1+(k1v1)*(dt/2d0)
der_th2=der_th2+(k1v2)*(dt/2d0)

a=(m2*(l2**2)*(der_th2**2)*sin(th1-th2))+((m1+m2)*l2*g*sin(th1))
b=(m2*l1*l2*(der_th1**2)*sin(th1-th2)*cos(th1-th2))-(m2*l2*g*sin(th2)*cos(th1-th2))
c=-((m1+m2)*l1*l2)+(m2*l1*l2*(cos(th1-th2)**2))
d=a+b

k2v1= (d/c )-(q1)*(der_th1)+fd1*sin(wd1*t)
k2v2= (((l1*(der_th1**2)*sin(th1-th2))-(l1*(k1v1)*cos(th1-th2))-(g*sin(th2)))/l2)!-(q1)*(der_th1)+fd1*sin(wd1*t)

der_th1=der_th1+(k2v1)*(dt/2d0)
der_th2=der_th2+(k2v2)*(dt/2d0)

a=(m2*(l2**2)*(der_th2**2)*sin(th1-th2))+((m1+m2)*l2*g*sin(th1))
b=(m2*l1*l2*(der_th1**2)*sin(th1-th2)*cos(th1-th2))-(m2*l2*g*sin(th2)*cos(th1-th2))
c=-((m1+m2)*l1*l2)+(m2*l1*l2*(cos(th1-th2)**2))
d=a+b

k3v1= (d/c )-(q1)*(der_th1)+fd1*sin(wd1*t)
k3v2= (((l1*(der_th1**2)*sin(th1-th2))-(l1*(k1v1)*cos(th1-th2))-(g*sin(th2)))/l2)!-(q1)*(der_th1)+fd1*sin(wd1*t)

der_th1=der_th1+(k3v1)*(dt/2d0)
der_th2=der_th2+(k3v2)*(dt/2d0)

a=(m2*(l2**2)*(der_th2**2)*sin(th1-th2))+((m1+m2)*l2*g*sin(th1))
b=(m2*l1*l2*(der_th1**2)*sin(th1-th2)*cos(th1-th2))-(m2*l2*g*sin(th2)*cos(th1-th2))
c=-((m1+m2)*l1*l2)+(m2*l1*l2*(cos(th1-th2)**2))
d=a+b

k4v1= (d/c )-(q1)*(der_th1)+fd1*sin(wd1*t)
k4v2= (((l1*(der_th1**2)*sin(th1-th2))-(l1*(k1v1)*cos(th1-th2))-(g*sin(th2)))/l2)!-(q1)*(der_th1)+fd1*sin(wd1*t)

der_v1=(1d0/6d0)*(k1v1+2*k2v1+2*k3v1+k4v1)
der_v2=(1d0/6d0)*(k1v2+2*k2v2+2*k3v2+k4v2)

v1=v1+der_v1*dt
v2=v2+der_v2*dt



!print *, Etot, counter


K1=(1d0/2.0d0)*(m1)*(l1**2)*(v1**2)
U1=(-g)*(m1)*(l1)*cos(th1)
E1=K1+U1

K2=(m2/2d0)*((l1**2)*(v1**2)+(l2**2)*(v2**2)+2d0*l1*l2*v1*v2*cos(th1-th2))
U2= (-m2*g)*(l1*cos(th1)+l2*cos(th2))
E2=K2+U2 

Etot=E1+E2 

x1=l1*sin(th1)
y1=-l1*cos(th1)
x2=x1+l2*sin(th2)
y2=y1-l2*cos(th2)

degth1=(th1)*(180d0/pi)
degth2=(th2)*(180d0/pi)

if(mod(counter,(lines/6000))==1) then

write (1,*) t,th1,th2, degth1,E1,K1,U1,Etot
write (2,*) x1,y1

write (3,*) t,v2, degth2,E2,K2,U2,Etot
write (4,*) x2,y2
 end if 
 
 
 
 
 
t=t+dt




end do






close(1)
close(2)
close(3)
close(4)
end program 


