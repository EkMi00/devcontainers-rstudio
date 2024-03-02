function fx = goldenfun(x,funparms)

% The unimodal function
if x <= 1
   fx = 1 - x;                                   % 0 <= x <= 1
elseif x <= 5
   fx = 0.5*(0.5*x-3).^2 + 0.9*(x-1) - 3.125;    % 1 <= x <= 5
else
   fx = f2(4.5) + 0.3*(x-4.5);                   % 5 <= x <= 6
end

end
