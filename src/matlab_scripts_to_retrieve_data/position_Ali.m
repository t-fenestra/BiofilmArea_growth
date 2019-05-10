function YY=position_Ali(ImageFolder,files,Threshold)
% size of the probe in pixels
probe_size=21;
Nfiles=length(files);
minFrame=min(15,Nfiles)
AliYY=zeros(minFrame,1);


for i=1:minFrame
    %Nfiles
    %----- read one image in folder----%
    imfile = fullfile(ImageFolder,files{i});
    grayImage = imread(imfile);
    %figure('Name','RGB');imshow(rgbImage)

    %------ apply threshold -------------%
    img=grayImage>Threshold;

    %------ delete probe ----------------%
    img(1:probe_size,1:probe_size)=0;
    %figure('Name','Threshold');imshow(img)
    %imshow(img);
    %hold on;
    
    [Y,X]=ind2sub(2048,find(img==1));
    
    if(~isempty(Y))
        AliYY(i)=min(Y);
    
         %--------- plot line -----------------%
%          YY=repmat(AliYY(i),2048,2);
%          XX=1:2048;
%          plot(XX,YY,'LineWidth',3,'color','red')
    end
    
    YY=min(AliYY(AliYY>0));
end
