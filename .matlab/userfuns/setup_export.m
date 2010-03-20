% force to export as vector
%set(gcf, 'Renderer', 'painters');
H = gcf;

allLines  = findall(H, 'type', 'line');
allImages = findall(H, 'type', 'image');
allLights = findall(H, 'type', 'light');
allPatch  = findall(H, 'type', 'patch');
allSurf   = findall(H, 'type', 'surface');
allRect   = findall(H, 'type', 'rectangle');
allAxes   = findall(H, 'type', 'axes');
allText   = findall(H, 'type', 'text');
allFont   = [allText; allAxes];
allColor  = [allLines; allText; allAxes; allLights];
allMarker = [allLines; allPatch; allSurf];
allEdge   = [allPatch; allSurf];
allCData  = [allImages; allPatch; allSurf];

set(allFont,'FontSize',18);
set(allText, 'interpreter', 'latex');
set(allLines, 'LineWidth', 2);
