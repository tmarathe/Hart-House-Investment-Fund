% Example Script MS_Regress_Fit.m

clear;

addpath('m_Files'); % add 'm_Files' folder to the search path
addpath('data_Files');

logRet=importdata('Example_Fex.txt');  % load some Data.

dep=logRet(:,1);                    % Defining dependent variable from .mat file
constVec=ones(length(dep),1);       % Defining a constant vector in mean equation (just an example of how to do it)
indep=[constVec];                   % Defining some explanatory variables
k=2;                                % Number of States
S=[1 1 1];                          % Defining which ones from indep will have switching effect (in this case variable 1 (constant), the variance and the GED parameter)
advOpt.distrib='GED';               % The Distribution assumption ('Normal', 't' OR 'GED')
advOpt.std_method=1;                % Defining the method for calculation of standard errors

[Spec_Out]=MS_Regress_Fit(dep,indep,k,S,advOpt); % Estimating the model

rmpath('m_Files');
rmpath('data_Files');