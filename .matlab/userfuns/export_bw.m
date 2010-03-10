%%
% Exports a figure in fig, black & white eps and pdf formats.
function export_bw (name)
    setup_export;
    
    saveas(gcf, name);
    saveas(gcf, name, 'eps2');
    system(['epstopdf ' name '.eps']);
