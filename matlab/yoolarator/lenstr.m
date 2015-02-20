function a=lenstr(x);
% LENSTR  Calculates the length of a string
%
%    Simple command that examines a string for the last
%    instance of a character other than space in it and
%    then returns the position of this character.
%
%    Written to help in cases where a title or caption
%    string has spaces in it that the user does not want
%    used when plotting.
%
%      >> a = lenstr('blah blah blah   ');
%    
%    This example will return a = 14 (the string itself
%    is 17 characters long).
%
%    Andrew Yool (axy@soc.soton.ac.uk), 13th June 2000.

temp=size(x);
temp2=max(size(temp));

% Check for silly strings
if temp2 > 2
error ' This input string has too many dimensions '
end

for i=1:1:temp(1)
flag=0; j=temp(2);
while flag==0
if x(i,j)==' '
	j=j-1;
	if j<1
		flag=1; answer=0;
	end
else
	flag=1; answer=j;
end
end

a(i)=answer;

end
