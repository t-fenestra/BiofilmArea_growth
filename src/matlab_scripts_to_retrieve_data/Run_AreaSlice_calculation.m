%% parameters of calculation

% pixel size
dx=0.160;
conv_coeff=2048/2048;
pixel_size=dx*conv_coeff;

% common threshold
Threshold=20.0*100;


% size of the probe in pixels
probe_size=21;

% layer in px
LayerSize=200; %px 32 mkm

% size of image
sizX=2048;
sizY=2048;

%% Read folder list

MainFolder='/Volumes/mpistaff/Diaz_Pichugina_Pseudomona/Data/1-TIMELAPSES_2019_1-1';
DirContent=dir(MainFolder);

% discard hidden folder and Dicarded trajectories
DirList=setdiff({DirContent.name},{'.','..','.DS_Store','Discarded'});

% Create emply table
ResultTable_SliceX=array2table(zeros(0,7),'VariableNames',{'Experiment','datafile','Layer','SumSliceX','SumSliceXsame','SumSliceXgain','SumSliceXlost'});
ResultTable_SliceY=array2table(zeros(0,7),'VariableNames',{'Experiment','datafile','Layer','SumSliceY','SumSliceYsame','SumSliceYgain','SumSliceYlost'})


%Files to write results
OutputFolderLocation='/Users/pichugina/Work/Data_Analysis/Occupied_area_analysis/BiofilmArea_growth/data/'
ResultFolder=fullfile(OutputFolderLocation,'Images')
mkdir(ResultFolder)

%% Read files and perform calculation
counterX=1; % used to fill the table
counterY=1;

for k=1:length(DirList)

    % Read file list in directory
    fprefix=DirList{k}
    
    ImageFolder=fullfile(MainFolder,fprefix);
    filePattern = fullfile(ImageFolder, '*Projection_.tif');
    TifFiles = dir(filePattern);
    files={TifFiles.name};
    

    if (~isempty(files))     % check if the file list empty to avoid mistake
        % sort files according to the data frame
        files=natsortfiles(files);
        Nfiles=length(files);
        
        % Aproximate position of ALI from first 10 files
        AliYY=position_Ali(ImageFolder,files,Threshold);
        NslX=floor(sizX/LayerSize); % number of layers in X-directioin
        NslY=floor((sizY-AliYY)/LayerSize); % number of layers in Y-direction
        
        
        %Create folder to save marked by gain and lost images
        mkdir(ResultFolder,fprefix)
        %Old=cd(fullfile(ResultFolder,fprefix))
        %mkdir(strcat(fprefix,'_img_gain_lost'));
        %cd(Old)
        FolderWithImages=fullfile(ResultFolder,fprefix,strcat(fprefix,'_img_gain_lost'));
        FolderWithImages=fullfile(ResultFolder,fprefix);

        
        
        % Calculate Area slice across folder
        for i=1:Nfiles
            
            %----- read one image in folder----%
            imfile = fullfile(ImageFolder,files{i});
            grayImage = imread(imfile);
            %figure('Name','RGB');imshow(rgbImage)

            %----- select blue channel ----------%
            %grayImage = rgbImage(:, :, 3);
            %[sizX,sizY]=size(grayImage )
            %figure('Name','Blue chanell');imshow(grayImage)

            %------ apply threshold -------------%
            img=grayImage>Threshold;

            %------ delete probe ----------------%
            img(1:probe_size,1:probe_size)=0;
            %figure('Name','Threshold');imshow(img)
            
            %------ sum area --------------------%
            LayerNameX=LayerSize:LayerSize:sizX;
            LayerNameY=LayerSize:LayerSize:(NslY*LayerSize); %counted from ALI
            [SumAreaSliceX,SumAreaSliceY]=Sum_slice_from_ali(img,LayerSize,AliYY,NslY,NslX);

             %------ overlay gain and lost in area --%
             if (i>1)
                 gain=img-prev_img>0;
                 [SumAreaSliceX_gain,SumAreaSliceY_gain]=Sum_slice_from_ali(gain,LayerSize,AliYY,NslY,NslX);

                 lost=img-prev_img<0;
                 [SumAreaSliceX_lost,SumAreaSliceY_lost]=Sum_slice_from_ali(lost,LayerSize,AliYY,NslY,NslX);
                 
                 same=(img==1 & (img-prev_img)==0);
                 [SumAreaSliceX_same,SumAreaSliceY_same]=Sum_slice_from_ali(same,LayerSize,AliYY,NslY,NslX);
                 
                 
                 prev_img=img;
                 
                 imshow(img,'Border','tight'); % 'InitialMag', 'fit',
                 
                   %% Make a truecolor all-green image.
                 green = cat(3, zeros(sizX,sizY), ones(sizX,sizY), zeros(sizX,sizY));
                 hold on;
                 h=imshow(green);

                 % Make a truecolor all-green image.
                 red = cat(3, ones(sizX,sizY),zeros(sizX,sizY),zeros(sizX,sizY));
                 g=imshow(red);

                 hold off;
                 set(h,'AlphaData',gain)
                 set(g,'AlphaData',lost)

                  hold on
                  YY=repmat(AliYY,sizY,2);
                  XX=1:sizX;
                  plot(XX,YY,'LineWidth',3,'color','yellow')
                  hold off;
                  
                  fileplace=fullfile(FolderWithImages,sprintf('%s_gain_lost.png',files{i}))
                  saveas(gcf,fileplace);
                   
                   datafile=strsplit(files{i},'.tif'); 
                   ExperimentX=repmat(string(fprefix),NslX,1);
                   ExperimentY=repmat(string(fprefix),NslY,1);
                   DataFileNameX=string(repmat(datafile{1},NslX,1));
                   DataFileNameY=string(repmat(datafile{1},NslY,1)); 
                   
                   T2 = table(ExperimentX,DataFileNameX,LayerNameX',SumAreaSliceX', SumAreaSliceX_same', SumAreaSliceX_gain',SumAreaSliceX_lost','VariableNames',{'Experiment','datafile','Layer','SumSliceX','SumSliceXsame','SumSliceXgain','SumSliceXlost'});
                   ResultTable_SliceX=[ResultTable_SliceX;T2];

                   T1 = table(ExperimentY,DataFileNameY,LayerNameY',SumAreaSliceY', SumAreaSliceY_same',SumAreaSliceY_gain',SumAreaSliceY_lost','VariableNames',{'Experiment','datafile','Layer','SumSliceY','SumSliceYsame','SumSliceYgain','SumSliceYlost'});
                   ResultTable_SliceY=[ResultTable_SliceY;T1];
             else
                 prev_img=img;
             end;
        end

    end
    
end
%%

fileSliceAreaY=fullfile(OutputFolderLocation,'ResultTable_SliceY.txt');
writetable(ResultTable_SliceY,fileSliceAreaY);
 
fileSliceAreaX=fullfile(OutputFolderLocation,'ResultTable_SliceX.txt');
writetable(ResultTable_SliceX,fileSliceAreaX);
