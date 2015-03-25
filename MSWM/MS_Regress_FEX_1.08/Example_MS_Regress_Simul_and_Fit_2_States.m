% Example Script MS_Regress_Fit.m and MS_Regress_Simul.m
% Script for simulating a 2 state MS regression model and then fit it with MS_Regress_Fit. 
% Just press f5 to run it..

addpath('m_Files'); % add 'm_Files' folder to the search path

clear; 

nr=1000;                    % Number of observations in simulation
advOpt.distrib='Normal';   % The distribution assumption ('Normal' or 't')

advOpt.std_method=1;        % Defining the method for calculation of standard errors

Coeff.p=[.95 .05  ; ...    % Transition matrix (this also defines the value of k)
         .05 .95 ];

Coeff.S=[1 1 0 0];  % Setting up which variables at indep will have switching effect

% Setting up the coefficients at non switching parameters (each row is each
% variables coefficient). The order is the same as Coeff.S

Coeff.nS_param(1,1)= .2;    % Setting up the coefficients at non switching parameters 
Coeff.nS_param(2,1)=-.2;    % Setting up the coefficients at non switching parameters 

% Setting up the coefficients at non switching parameters (each row is each
% variables coefficient and each collum is each state). This example has
% two switching parameters and 2 states

Coeff.S_param(:,1)=[ .5  .1];    % Setting up the coefficients at switching parameters  
Coeff.S_param(:,2)=[-.3 -.2];    % Setting up the coefficients at switching parameters 

Coeff.Std(1,1)=.03;  % Setting up the standard deviavion of the model at State 1
Coeff.Std(1,2)=.02;  % Setting up the standard deviavion of the model at State 2

% The explanatory variables are going to be random normal ones, so the mean and std of
% each simulated variable is needed. 

Coeff.indepMean=[.2 .0 .1  0];  % Setting up the mean of independent (explanatory) variables 
Coeff.indepStd= [.1 .2 .2 .1];  % Setting up the mean of independent (explanatory) variables 

k=size(Coeff.p,1);  % getting the value of k, according to Coeff.p

figure(1); % making sure matlab plots both figures
[Simul_Out]=MS_Regress_Sim(nr,Coeff,k,advOpt.distrib); % calling simulation function

% passing values to the arguments of the fit function

dep=Simul_Out.dep;
indep=Simul_Out.indep;
k=Simul_Out.k;
S=[Simul_Out.Coeff.S 1];    % Adding the extra dummy for variance switching

figure(2);
[Spec_Output]=MS_Regress_Fit(dep,indep,k,S,advOpt);

rmpath('m_Files');