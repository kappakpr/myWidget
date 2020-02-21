%%% Denoising using splines %%%
%2D example: Generate data
n = 100;
sigma = 0.5;
Y = peaks(n) + randn(n)*sigma;
figure
imagesc(Y)
%2D Spline
k = 10;
sd = 4;
knots = [ones(1,sd-1)...
    linspace(1,n,10) n * ones(1,sd-1)];
nKnots = length(knots) - sd;
kspline = spmak(knots,eye(nKnots));
H = cell(2,1); B=cell(2,1);
for i = 1:2
    B{i}=spval(kspline,1:n)';
    H{i} = B{i}/(B{i}'*B{i})*B{i}';
end
Yhat = H{2}*Y*H{1};
figure
imagesc(Yhat)