addpath ~/.matlab/
addpath ~/.matlab/userfuns
addpath ~/.matlab/userfuns/ezyfit
efmenu
% please dock all my figures, thank you. This way, matlab stops popping annoying figures everywhere
if(any(get(0, 'MonitorPositions') ~= [0 0 1 1]))
    % set(0,'defaulttextfontsize',18)
    % set(0,'defaultaxesfontsize',18)
    set(0, 'DefaultFigureWindowStyle', 'docked')
end
