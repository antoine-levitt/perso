addpath ~/.matlab/
addpath ~/.matlab/userfuns
% please dock all my figures, thank you. This way, matlab stops popping annoying figures everywhere
if(get(0, 'MonitorPositions') ~= [0 0 1 1])
    set(0, 'DefaultFigureWindowStyle', 'docked')
end
