%%
% Exports a figure in fig, eps and pdf formats.
function export (name)
    saveas(gcf, name);
    saveas(gcf, name, 'epsc');
    system(['epstopdf ' name '.eps']);
