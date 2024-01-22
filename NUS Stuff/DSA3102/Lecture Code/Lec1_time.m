

%1D transformation
xx=0:0.1:24;
ff=xx.*sqrt(24-xx);
figure
plot(xx,ff);
% ff=xx.^2;
% yy=xx-0.25;
% zz=-2*(xx+1)+1;
% hold on
% plot(xx,ff,xx,yy,xx,zz)

%Time management example
x=0:.2:24;
y=0:.2:24;
[xx,yy]=meshgrid(x,y);
ff=xx.*sqrt(yy);%Example 1.2
region=(xx+yy==24);
region=double(region)*100;
figure
surf(xx,yy,ff)
colormap(jet)
hold on
surf(xx,yy,region)
shading interp 
hold off

figure
contour(xx,yy,ff,10,'ShowText','on')
hold on
contour(xx,yy,region,1,'--')
hold off





% xx=-3:0.1:3;
% ff=xx.^2-2*xx;
% figure
% plot(xx,ff)
% 
% ff=xx.^3-3*xx;
% figure
% plot(xx,ff)
% 
% xx=-5:0.01:5;
% figure 
% ff=xx.^2+4.*cos(xx.^2);
% plot(xx,ff)
% 
% x=-1:0.1:1;
% y=x;
% [xx,yy]=meshgrid(x,y);
% ff=xx.^2-yy.^2;
% figure
% surf(xx,yy,ff)
% colormap(jet)
% figure
% contour(xx,yy,ff)




