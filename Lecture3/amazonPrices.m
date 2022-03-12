function y = amazonPrices()
opts = detectImportOptions('AMZN.csv');
opts.DataLines = 2;
opts.VariableNamesLine = 1;
T = table2array(readtable('AMZN.csv',opts))
col1 = T(:, 1);
subplot(211)
histfit(col1)
fitdist(col1, "Normal")