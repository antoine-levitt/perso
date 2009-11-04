%%
% Exports a figure in fig, black & white eps and pdf formats.
function export_bw (name)
    saveas(gcf, name);
    saveas(gcf, name, 'eps');
    system(['epstopdf ' name '.eps']);
