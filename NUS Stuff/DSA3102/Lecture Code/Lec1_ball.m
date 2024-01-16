%Example 1.1+1.2
eps=0.1;
x=-1:eps:8;
y=-1:eps:9;
[xx,yy]=meshgrid(x,y);
ff=(xx-4).^2+(yy-6).^2;%Example 1.1
%ff=(xx-2).^2+(yy-2).^2;%Example 1.2
region=(xx>=0)&(yy>=0)&(xx<=4)&(yy<=6)&(3*xx+2*yy<=18);
%region=min(min(min(min((xx>=0),(yy>=0)),(xx<=4)),(yy<=6)), (3*xx+2*yy<=18));
region=double(region)*20-20;
figure
surf(xx,yy,ff)
colormap(jet)
hold on
surf(xx,yy,region)
shading interp 
hold off

figure
contour(xx,yy,ff,0:1:4,'ShowText','on')
hold on
contour(xx,yy,region,1,'--')


% [gx,gy]=gradient(ff,eps,eps);
% 
% quiver(x,y,gx,gy)
hold off




