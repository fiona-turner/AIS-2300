% Return the mean, and 17-83% intervals of normal distribution, given the
% 5-95% confidence interval

l5 = -0.6;
u5 = 4.4;


dist_mean = mean([l5, u5]);
dist_sd = (u5 - l5)/(2*1.645); %95% is 1.645 standard deviations
likely_range = [dist_mean - dist_sd, dist_mean + dist_sd]

