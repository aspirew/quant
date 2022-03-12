function y = checkNormal()
Array=csvread('to_check.csv');
col1 = Array(:, 1);
subplot(211)
histfit(col1)
fitdist(col1, "Normal")