function progress(i, n)
%notify user of progress. Assume some process from i to n
    persistent lasttime lasti;

    if (~(isempty(lasttime)))
        percent = i/n*100;
        
        timeleft = (n-i)*(cputime - lasttime)/(i-lasti);
        fprintf('%4.2f percent, %s left\n', percent, sec2str(floor(timeleft)));
    end
    lasttime = cputime;
    lasti = i;

% This part not by me
function [hour, minute, second] = sec2hms(sec)
%SEC2HMS  Convert seconds to hours, minutes and seconds.
%
%   [HOUR, MINUTE, SECOND] = SEC2HMS(SEC) converts the number of seconds in
%   SEC into hours, minutes and seconds.

%   Author:      Peter J. Acklam
%   Time-stamp:  2002-03-03 12:50:09 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

    hour   = fix(sec/3600);      % get number of hours
    sec    = sec - 3600*hour;    % remove the hours
    minute = fix(sec/60);        % get number of minutes
    sec    = sec - 60*minute;    % remove the minutes
    second = sec;

function res = sec2str(sec)
    [h m s] = sec2hms(sec);
    if (h ~= 0)
        res = sprintf('%dh %dm %ds', h, m, s);
    elseif(m ~= 0)
        res = sprintf('%dm %ds', m, s);
    else
        res = sprintf('%ds', s);
    end
