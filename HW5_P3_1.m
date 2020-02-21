load ('movie_data');
rateMatrix = train;

%%%%%%%%%%%%%%%%%% Matrix Completion %%%%%%%%%%%%%%%%%%

%%% Example %%%
%Generating original matrix
n1 = 943; n2 = 1682;
A = rateMatrix; %%randi([-20,20],n1,n2);
r = 2;
[U, S, V] = svd(A);
if n1 < n2
    s = diag(S); s(r+1:end)=0; S=[diag(s) zeros(n1,n2-n1)];
else
    s = diag(S); s(r+1:end)=0; S=[diag(s); zeros(n1-n2,n2)];
end
X = U* S* V';
X0 = X;
%Removing 20% of the observations
%A = [rand(n1,n2)>=0.80];
%X(A) = 0;
m = sum(sum(A==0));
%Initialization
Y=zeros(n1,n2);
delta = n1*n2/m;
tau = 250;
%Iterations
vec = zeros(500,1);
for i = 1:500
    fprintf('Iteration -%d\n', i);
    [U, S, V] = svd(Y);
    S_t = (S-tau);
    S_t(S_t<0) = 0;
    Z = U*S_t*V';
    P = X-Z;
    %P(A) = 0;
    Y0 = Y;
    Y = Y0 + delta*P;
    vec(i) = sum(sum((Y-Y0).^2));
    err(i)=sum(sum((X0-Z).^2))/sum(sum((X0).^2));
end
% plot the results
figure;plot(vec);
figure;plot((err));
figure;
Ar=reshape(A, n1*n2,1);
Xr=reshape(X0, n1*n2,1);%Xr=Xr(Ar);
Zr=reshape(Z, n1*n2,1);%Zr=Zr(Ar);
subplot(2,1,1);plot(Xr);hold on;plot(Zr,'r');
subplot(2,1,2);plot(Xr-Zr);
figure;
imagesc(Z)
figure;
imagesc(X0)
