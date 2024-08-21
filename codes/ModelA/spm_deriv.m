function spm_deriv(con1, con2, filename)
% --------------------------------------------------------------------------------------
% Script to include temporal derivative
% Created by EAE 9-17-03
%-------------------------------------------------------------------------------------


%------------------Variables to Change Listed Below--------------------------------------------

%   mainEffectFileConvention = 'conFile.img';
%   derivativeFileConvention = 'conFile2.img';
mainEffectFile = con1;
derivativeFile = con2;
outputfile = filename;
%----------------------- Don't Change anything below this line --------------------------


Q = outputfile;
P = str2mat(mainEffectFile, derivativeFile);

% Make sure Files Exist Before Running Image Calc
for i=1:size(P,1)
  if(~exist(deblank(P(i,:)),'file'))
    error(['File Does Not Exist',P(i,:)]);
  end
end


% calculate new images
for i=1:size(Q,1)
  index=i*2;
  disp('Calculating images for files');
  disp(P([index-1 index],:));

  %get input file names
  con_file(1,:)=deblank(P(index-1,:));
  con_file(2,:)=deblank(P(index,:));
  output_file= deblank(Q(i,:));

  %load input files and reshape
  Vme = spm_vol(con_file(1,:));
  me=spm_read_vols(spm_vol(con_file(1,:)));
  dme=spm_read_vols(spm_vol(con_file(2,:)));

%   out = make_deriv(dme,me); % GSPP: dme is the derivative and me is the main effect
  out = make_deriv(me,dme);   % GSPP: I inverted the order here

  Vi = spm_vol(char(con_file(1,:)));
  if isempty(Vi), error('no input images specified'), end

  %-Work out filename for output image
  %------------------------------------------------------------------
  Qdir = spm_str_manip(output_file,'H');
  Qfil = [spm_str_manip(output_file,'stv'),'.img'];
  if ~exist(Qdir,'dir')
    warning('Invalid directory: writing to current directory')
    Qdir = '.';
  end
%   output_file = spm_select('CPath',Qfil,Qdir); % GSPP changed it

  type=4;
  Vo = Vme;
  Vo.fname = output_file;
  Vo.descrip = 'tempDeriv + main effect contrast';
%   Vo = struct(	'fname',	output_file,...
%     'dim',		[Vi(1).dim(1:3),type],...
%     'mat',		Vi(1).mat,...
%     'descrip',	'tempDeriv + main effect contrast');

  Vo = spm_write_vol(Vo,out);

  disp(['New Image Saved to ',Vo.fname]);
  disp(' ');
  clear con_file;clear output_file;
end