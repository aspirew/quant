function y = vol0Check()
Array=csvread('to_check.csv');
col1 = Array(1,:);
subplot(211)
histogram(col1)