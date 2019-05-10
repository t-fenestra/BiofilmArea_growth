function [SumSliceX,SumSliceY]=Sum_slice_from_ali(img,LayerSize,AliYY,NslY,NslX)
    % assumming that image is a square size
    slice=1:LayerSize;
    SumSliceX=zeros(1,NslX);
    SumSliceY=zeros(1,NslY);
    
    % slice in X-direction
    start=0;
    for k=1:NslX
        curent_slice=start+slice;
        start=start+LayerSize;
        SliceX=img(:,curent_slice);
        SumSliceX(k)=sum(SliceX(:));
        %imshow(SliceX);
    end
    
    % slice in Y-direction
    start=AliYY;
    for m=1:NslY
        curent_slice=start+slice;
        start=start+LayerSize;
        SliceY=img(curent_slice,:);
        SumSliceY(m)=sum(SliceY(:));
        %imshow(SliceY);
        %123;
    end
%AspectRatioYX=SumSliceY./SumSliceX
    
