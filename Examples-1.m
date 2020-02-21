% EXAMPLES OF IMAGE ANALYSIS

%%%%%%%%%%%%%%%%%% IMAGE TRANSFORMATION %%%%%%%%%%%%%%%%%%

%%% Image histogram %%%
I = imread('Albert.png'); 
N = 256;
imhist(I,N)

%%% Histogram stretching %%%
figure
imshow(uint8(double(I)/205*255))
figure
imshow(uint8(double(I)/150*150))


%%%%%%%%%%%%%%%%%% IMAGE FILTTERING AND CONVOLUTION %%%%%%%%%%%%%%%%%%

%%% Image filtering %%%
Y = imread('Albert.png');
K = cell(6,1); 
K{1} = [1 0 -1; 0 0 0; -1 0 1];      %Edge Detection
K{2} = [0 1 0;1 -4 1;0 1 0];         %Edge Detection
K{3} = [-1 -1 -1;-1 8 -1;-1 -1 -1];  %Edge Detection
K{4} = [ 0 -1 0; -1 5 -1;0 -1 0];    %Sharpening 
K{5} = ones(3,3)/9;                  %Blurring
K{6} = [ 1 2 1;2 4 2;1 2 1]/16;      %Blurring 
Yk = cell(6,1); 
for i = 1:6
    Yk{i} = imfilter(Y,K{i});
    subplot(2,3,i)
    imshow(Yk{i})
    title(num2str(round(K{i},1)))
end

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


%%%%%%%%%%%%%%%%%% IMAGE SEGMENTATION %%%%%%%%%%%%%%%%%%

%%% Otsu's Method %%%
%Step 1: get the histogram of image
Y = imread('coins.png');
I = im2uint8(Y(:));
num_bins = 256;
counts = imhist(I,num_bins);
%Step 2: Calculate group mean
p = counts / sum(counts);
omega = cumsum(p);
mu = cumsum(p .* (1:num_bins)');
mu_t = mu(end);
%Step 3: find the maximum value of 
sigma_b_squared = (mu_t * omega - mu).^2 ./ (omega .* (1 - omega));
maxval = max(sigma_b_squared);
idx = mean(find(sigma_b_squared == maxval));
%Step 4: Thresholding and get final image
level = (idx - 1) / (num_bins - 1);
BW = Y > level*256;
figure, imshow(BW)

%%% K-means clustering %%%
%Load data
I = imread('coin.png'); 
imshow(I) 
X=reshape(I,size(I,1)*size(I,2),size(I,3));
X=double(X);
%Set parameters
K=2; 
max_iter = 100;
%Clustering
[N, d] = size(X);
% indicator matrix (each entry corresponds to the cluster of each point in X)
L = zeros(N, 1);
% centers matrix
C = zeros(K, d);
for i = 1:max_iter
    % step 1: optimize the labels
    dist = zeros(N,K);
    for j = 1:N
        for k = 1:K
            dist(j,k) = norm(X(j,:)-C(k,:))^2;
        end
    end
    [disto, index] = sort(dist,2);
    L = index(:,1);
    % step 2: optimize the centers
    for k = 1:K
        if sum(L == k)~= 0 
            C(k,:) = sum(X(L == k, :),1)/sum(L == k);
        end    
    end    
end
Y = reshape(L,size(I,1),size(I,2)); 
BW = Y == 1;
figure, imshow(BW)


%%% k-means clustering %%%
% input image
I = imread('CS.png'); 
imshow(I) 
X=reshape(I,size(I,1)*size(I,2),size(I,3));
% segmentation with different K values
K=[2 3 4 5];
for i = 1:4
[L,Centers] = kmeans(double(X),K(i));
Y = reshape(L,size(I,1),size(I,2)); 
B = labeloverlay(I,Y);
subplot (2,2,i);
imshow(B) 
end

%%%%%%%%%%%%%%%%%% EDGE DETECTION %%%%%%%%%%%%%%%%%%

%%% Sobel operator %%%
a = double(imread('coins.png'));  [m,n] = size(a); e = false(m,n);
op = [1 2 1; 0 0 0;-1 -2 -1]/8; x_mask = op';  y_mask = op;
fx = imfilter(a,x_mask,'replicate'); imagesc(fx)
fy = imfilter(a,y_mask,'replicate'); imagesc(fy)


