%%
% Exports a figure in fig, eps and pdf formats.
function export (name)
    setup_export;
    
    saveas(gcf, name);
    saveas(gcf, name, 'epsc2');
    system(['epstopdf ' name '.eps']);
