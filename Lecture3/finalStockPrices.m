function y = finalStockPrices()
Array=csvread('zad2.csv');
x = Array(:, 1);
% Make up some data. (You should use your real data in place of x.)
y = lognrnd(1,0.3,10000,1);
% Fit the data
parmhat = lognfit(x);
% Plot comparison of the histogram of the data, and the fit
figure
hold on
% Empirical distribution
hist(x,0.1:0.1:10);
% Fitted distribution
xt = 0.1:0.1:10;
plot(xt,1000*lognpdf(xt,parmhat(1),parmhat(2)),'r')