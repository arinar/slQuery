% test features of slQuery 

% preconditions
% open test model in fresh state
open_system('slQuery_testmodel');
modelDTor = onCleanup(@() close_system('slQuery_testmodel', false));

% special elements
subs22 = get_param('slQuery_testmodel/Subsystem', 'Handle'); % block chosen to have 2 inports, 2 outports

% parameter/attribute access:

%% wrap handle into object
assert(slQuery(gcbh) == gcbh);

%% the gcb trick (gcb)
assert(slQuery.gcb == gcbh);

%% the gcb trick (direct parameter access)
assert(isequal(slQuery.gcb.Name, get_param(gcb, 'Name')));

%% the gcb trick (gcs)
assert(slQuery.gcs == get_param(gcs, 'Handle'));

%% get block handle (scalar selection)
x = slQuery.gcb;
assert(x.Handle == gcbh);
assert(isnumeric(x.Handle) && ~isa(x.Handle, 'slQuery'));

%% get block handle (nonscalar selection)
X = slQuery(gcbh * [1 1; 1 1]);
assert(isequal(X.Handle, gcbh * [1 1; 1 1]));

%% get block full name (scalar selection)
x = slQuery.gcb;
assert(isequal(x.fullname, gcb));

%% get block full name (nonscalar selection)
X = slQuery(gcbh * [1 1; 1 1]);
assert(isequal(X.fullname, {gcb gcb; gcb gcb}));

%% wrap blockref into slQuery object (scalar)
x = slQuery.gcb;
assert(isa(x.Parent.wrap, 'slQuery'));

%% wrap blockref into slQuery object (nonscalar)
X = slQuery(gcbh * [1 1; 1 1]);
assert(isa(X.Parent.wrap, 'slQuery'));

%% wrap handle into slQuery object (scalar)
x = slQuery.gcb;
assert(isa(x.Parent.Handle.wrap, 'slQuery'));

%% wrap handle into slQuery object (nonscalar)
X = slQuery(gcbh * [1 1; 1 1]);
assert(isa(X.Parent.Handle.wrap, 'slQuery'));

%% block object parameters act as the properties
x = slQuery.gcb;
assert(isempty(setxor(properties(x), fieldnames(get_param(gcbh, 'ObjectParameters')))));

%% get block parameter (string)
x = slQuery.gcb;
assert(isequal(x.BackgroundColor, get_param(gcb, 'BackgroundColor')));

%% set block parameter (string)
x = slQuery.gcb;
fix = fix_param(gcb, 'BackgroundColor');
x.BackgroundColor = 'black';

%% get block parameter (scalar numeric)
x = slQuery.gcb;
assert(isequal(x.FontSize, get_param(gcb, 'FontSize')));

%% set block parameter (scalar numeric)
x = slQuery.gcb;
fix = fix_param(gcb, 'FontSize');
x.FontSize = 23;

%% get block parameter (rectangle)
x = slQuery.gcb;
assert(isequal(x.Position, get_param(gcb, 'Position')));

%% set block parameter (rectangle)
x = slQuery.gcb;
fix = fix_param(gcb, 'Position');
x.Position = [100, 200, 300, 400];

%% get block parameter (structure)
x = slQuery(subs22);
assert(isequal(x.LineHandles, get_param(subs22, 'LineHandles')));

%% get block parameter (field of a structure)
x = slQuery(subs22);
assert(isequal(x.LineHandles.Inport, getfield(get_param(subs22, 'LineHandles'), 'Inport')));

%% get block parameter (element of an array-field of a structure)
x = slQuery(subs22);
assert(isequal(x.LineHandles.Inport(2), getfield(get_param(subs22, 'LineHandles'), 'Inport', {2})));

% selectors:
%% any selector
X = slQuery('*');
assert(~isempty(X));

%% block type selector
X = slQuery('SubSystem');
assert(~isempty(X));
assert(all(strcmp(X.BlockType, 'SubSystem')));

%% block type selector (none found)
assert(isempty(slQuery('NotABlockType')));

%% block name selector
X = slQuery('#In1');
assert(~isempty(X));
assert(all(strcmp(X.Name, 'In1')));

%% block name selector (none found)
assert(isempty(slQuery('#NotABlockName')));

%% mask type selector
X = slQuery('.TestMask');
assert(~isempty(X));
assert(all(strcmp(X.MaskType, 'TestMask')));

%% mask type selector (none found)
assert(isempty(slQuery('.NotABlockMask')));

%% pseudo-class selector
X = slQuery('+TreatAsAtomicUnit');
assert(~isempty(X));
assert(all(strcmp(X.TreatAsAtomicUnit, 'on')));

%% pseudo-class selector (none found)
assert(isempty(slQuery('+MinAlgLoopOccurrences'))); % no subsystem is set this way

%% param spec selector (plain)
X = slQuery('[type=block_diagram]');
assert(~isempty(X));
assert(all(strcmp(X.type, 'block_diagram')));

%% param spec selector (start)
X = slQuery('[Name^=In1]');
assert(~isempty(X));
assert(~any(cellfun(@isempty, regexp(X.Name, '^In1', 'match', 'once'))));

%% param spec selector (end)
X = slQuery('[Name$=11]');
assert(~isempty(X));
assert(~any(cellfun(@isempty, regexp(X.Name, '11$', 'match', 'once'))));

%% param spec selector (part)
X = slQuery('[Name*=sig]');
assert(~isempty(X));
assert(~any(cellfun(@isempty, regexp(X.Name, 'sig', 'match', 'once'))));

%% param spec selector (full regex)
X = slQuery('[BlockType~=^(In|Out)port$]');
assert(~isempty(X));
assert(all(ismember(X.BlockType, {'Inport', 'Outport'})));

%% param spec selector multiple params
X = slQuery('[BlockType=Inport,BackgroundColor=orange]');
assert(~isempty(X));
assert(all(strcmp(X.BlockType, 'Inport')));
assert(all(strcmp(X.BackgroundColor, 'orange')));

%% argidx selectors (single char)
x = slQuery('(1)', gcb);
assert(x == gcbh);

%% argidx selectors (single handle)
x = slQuery('(1)', gcbh);
assert(x == gcbh);

%% argidx selectors (cellstr candidate list)
x = slQuery('(1)', {gcb}); % all these blocks must exist TODO: it'd be better if they hadn't have to
assert(x == gcbh);

%% argidx selectors (handle candidate list)
x = slQuery('(1)', [gcbh, -0.42, -0.23]); % nonexisting handles won't be matched
assert(x == gcbh);

%% combined selectors (wildcard and param spec)
X = slQuery('*[Name^=Subsystem]');
assert(~isempty(X));
assert(~any(cellfun(@isempty, regexp(X.Name, '^Subsystem', 'match', 'once'))));

%% combined selectors (block type and block name)
X = slQuery('Terminator#Terminator');
assert(~isempty(X));
assert(all(strcmp(X.BlockType, 'Terminator')));
assert(all(strcmp(X.Name, 'Terminator')));

%% combined selectors (wildcard, block type, block name, mask type, param spec and pseudo class)
X = slQuery('*Terminator#Terminator.T900[BackgroundColor=white]+ShowName');
assert(~isempty(X));
assert(all(strcmp(X.BlockType, 'Terminator')));
assert(all(strcmp(X.Name, 'Terminator')));
assert(all(strcmp(X.MaskType, 'T900')));
assert(all(strcmp(X.BackgroundColor, 'white')));
assert(all(strcmp(X.ShowName, 'on')));

%% port handle access with colon operator (inport)
for x = slQuery('Mux')
	assert(x.PortHandles.Inport(1) == 1:x);
	assert(x.PortHandles.Inport(2) == 2:x);
end

%% port handle access with colon operator (outport)
for x = slQuery('Demux')
	assert(x.PortHandles.Outport(1) == x:1);
	assert(x.PortHandles.Outport(2) == x:2);
end

%% join combinator
assert(~isempty(slQuery('*, *')));

%% backref selector does not yield a column
x = slQuery('SubSystem, $1')';
assert(size(x, 1) == 1);

%% backref selector, more uses
x = slQuery('SubSystem, $1, Terminator, $2, $1')';
assert(size(x, 1) == 2);

% hierarchy-combinators
%% shape of the result
assert(size(slQuery('Goto, From'), 1) == 2);

%% shape of an empty result
assert(isequal(size(slQuery('SubSystem, GotoTagVisibility, Nonsense')), [3, 0]));

%% use result in loop
for x = slQuery('Goto, From')
	assert(size(x, 1) == 2);
	assert(size(x, 2) == 1);
end

%% assign query to separate variables (')
[B,F] = slQuery('Goto, From')';
assert(size(B, 1) == 1);
assert(size(F, 1) == 1);

%% numeric indexing for columns (single column)
X = slQuery('Goto, From');
assert(isequal(X(1), X(1, :)));

%% numeric indexing for columns (multiple columns)
X = slQuery('Goto, From, SubSystem');
assert(isequal(X([1, 3]), X([1 3], :)));

%% numeric indexing for columns (get parameter)
X = slQuery('Goto, From');
assert(isequal(X(1).BlockType, repmat({'Goto'}, size(X(1)))));

%% numeric indexing for columns (set parameter, distribute scalar)
X = slQuery('Goto, From');
X(1).BackgroundColor = 'white';
X(2).BackgroundColor = 'white';

%% numeric indexing for columns (set parameter, distribute single column)
X = slQuery('Goto, From');
X([1 2]).BackgroundColor = {'green'; 'red'};
X([2 1]).BackgroundColor = {'white'; 'white'};

%% numeric indexing for columns (set parameter, distribute row as column)
X = slQuery('Goto, From');
fix = fix_param(double(X), 'BackgroundColor');
X([1 2]).BackgroundColor = {'green'; 'red'};

%% numeric indexing for columns (set parameter, distribute row)
X = slQuery('Goto, From');
fix = fix_param(double(X), 'BackgroundColor');
X([1 2]).BackgroundColor = repmat({'green'}, [1, size(X, 2)]);

%% logical indexing for rows
X = slQuery('Goto, From');
assert(isequal(X([true false true false true false]), X(:, [1 3 5])));

%% logical indexing for rows (get parameter)
X = slQuery('Goto, From');
assert(isequal(X([true false true false true false]).BlockType, repmat({'Goto'; 'From'}, [1 3])));

%% normal indexing (two indices) for exact element
X = slQuery('Goto, From');
assert(isequal(size(X(2,3)), [1 1]));

%% normal indexing (two indices) for exact element (get parameter)
X = slQuery('Goto, From');
assert(isequal(X(2,3).BlockType, 'From'));

%% child combinator
X = slQuery('SubSystem / Goto');
assert(~isempty(X));
for x = X
	assert(x(1).Handle == x(2).Parent.Handle)
end

%% descendents combinator
for x = slQuery('SubSystem // Goto')
	s = x(2);
	while s ~= 0
		s = s.Parent.Handle.wrap;
		if s == x(1), break, end
	end
	assert(s ~= 0);
end

%% parent combinator
X = slQuery('Goto \ SubSystem');
assert(all(X(1).Parent.Handle == X(2).Handle));

%% child and parent yield same info
X = slQuery('SubSystem / Goto \ SubSystem');
assert(isequal(X(1), X(3)));

%% ascendants combinator
[B, S] = slQuery('Goto \\ SubSystem')';
while any(S)
	B = B.Parent.Handle.wrap;
	i = B == S; % drop all blocks that match up
	B = B(~i); S = S(~i);
end
assert(isempty(B) && isempty(S));

%% sibling combinator
X = slQuery('Goto Goto');
assert(~isempty(X));
for x = X
	assert(x(1).Parent.Handle == x(2).Parent.Handle); % must have same parent
	assert(x(1) ~= x(2)); % can't be sibling to oneself
end

% signal lines, wires

%% signal line combinator (downstream)
for x = slQuery('Inport -> Outport')
	assert(x(1):1 == get_param(double(-1:x(2)), 'SrcPortHandle'));
end

%% signal line combinator (upstream)
for x = slQuery('Outport <- Inport')
	assert(x(2):1 == get_param(double(-1:x(1)), 'SrcPortHandle'));
end

% TODO: signal line combinator (indirectional)
%for x = slQuery('SubSystem - SubSystem')
%end

%% signal line combinators with portspec (port number, inport)
for x = slQuery('Inport -> 2:SubSystem:2 <- Inport')
	assert(x(1):1 == get_param(double(-2:x(2)), 'SrcPortHandle'));
	assert(x(1) == x(3));
end

%% signal line combinators with portspec (port number, outport)
for x = slQuery('Outport <- 2:SubSystem:2 -> Outport')
	assert(x(2):2 == get_param(double(-1:x(1)), 'SrcPortHandle'));
	assert(x(1) == x(3));
end

%% signal line combinators with portspec (port name, inport)
for x = slQuery('Inport -> In2:SubSystem:In2 <- Inport')
	i = str2double(get_param([x(2).fullname '/In2'], 'Port'));
	assert(x(1):1 == get_param(double(-(i):x(2)), 'SrcPortHandle'));
	assert(x(1) == x(3));
end

%% signal line combinators with portspec (port name, outport)
for x = slQuery('Outport <- Out2:SubSystem:Out2 -> Outport')
	i = str2double(get_param([x(2).fullname '/Out2'], 'Port'));
	assert(x(2):(i) == get_param(double(-1:x(1)), 'SrcPortHandle'));
	assert(x(1) == x(3));
end

%% downstream and upstream signal line combinators yield same info
for x = slQuery('Inport -> Outport <- Inport')
	assert(x(1) == x(3));
end

% dataflow wiring combinators - colored port blocks are set up in the test model so the port
% numbers and the numbers in their block names match the data flow correspondence

%% wiring combinator (downstream)
[i, o] = slQuery('Inport[BackgroundColor=orange] => Outport[BackgroundColor=lightBlue]')';
assert(~isempty(i));
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(i.Name, '\d+', 'match', 'once')))); 
assert(all(strcmp(i.Port, o.Port)));

%% wiring combinator (upstream)
[o, i] = slQuery('Outport[BackgroundColor=lightBlue] <= Inport[BackgroundColor=orange]')';
assert(~isempty(o));
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(i.Name, '\d+', 'match', 'once'))));
assert(all(strcmp(i.Port, o.Port)));

% TODO: wiring combinator (indirectional)
%for x = slQuery('Mux = Mux')
%end

% a few extra ports along the wires are places in subsystems, they are also named correctly

%% wiring combinator (downstream, including virtual blocks)
[i, o] = slQuery('Inport[BackgroundColor=orange] ~> Outport')';
assert(~isempty(i));
assert(any(strcmp(o.BackgroundColor, 'white')));
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(i.Name, '\d+', 'match', 'once')))); 

%% wiring combinator (downstream, including virtual blocks)
[o, i] = slQuery('Outport[BackgroundColor=lightBlue] <~ Inport')';
assert(~isempty(o));
assert(any(strcmp(i.BackgroundColor, 'white')));
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(i.Name, '\d+', 'match', 'once'))));

% TODO: wiring combinator (indirectional, including virtual blocks)
%for x = slQuery('Mux = Mux')
%end

% signal slicing combinators

% colored slicing subsystems are set up in the test model so the numbers in their blocks names
% match

%% signal slicing combinator (downstream)
[si, ~, ~, so] = slQuery('SubSystem[BackgroundColor=red] / Inport >> Outport \ SubSystem[BackgroundColor=green]')';
assert(all(strcmp(regexp(si.Name, '\d+', 'match', 'once'), regexp(so.Name, '\d+', 'match', 'once'))));

%% signal slicing combinator (upstream)
[so, ~, ~, si] = slQuery('SubSystem[BackgroundColor=green] / Outport << Inport \ SubSystem[BackgroundColor=red]')';
assert(all(strcmp(regexp(si.Name, '\d+', 'match', 'once'), regexp(so.Name, '\d+', 'match', 'once'))));

% TODO: signal slicing combinator (indirectional)
%for x = slQuery('Mux <> Mux')
%end

% testmodel uses white Memory blocks to break up algebraic loops
%% signal slicing combinator, detect feedback loops (downstream)
X = slQuery('Memory[BackgroundColor=white] >> $1');
assert(~isempty(X));

%% signal slicing combinator, detect feedback loops (upstream)
X = slQuery('Memory[BackgroundColor=white] << $1');
assert(~isempty(X));

% testmodel uses grey 'fake' Memory blocks to mark tricky non-loop situations
%% signal slicing combinator, looping bus, but not subsignal (downstream)
X = slQuery('Memory[BackgroundColor=grey] >> $1');
assert(isempty(X));

%% signal slicing combinator, looping bus, but not subsignal (upstream)
X = slQuery('Memory[BackgroundColor=grey] << $1');
assert(isempty(X));

% reference block

%% resolved link to a library block (acute)
X = slQuery('SubSystem ´ *');

%% all active links to a block (grave)
X = slQuery('slQuery_testlibrary // * ` SubSystem');

%% resolved link to a library block (international)
X = slQuery('SubSystem § *');

%% all active links to a block (international)
X = slQuery('slQuery_testlibrary // * @ SubSystem');

% combinatorics

%% combine results (common info)
% $2 is handled in groups, because it repeats
X = slQuery('Inport \ #Subsystem / Outport');
assert(isequal(size(X), size(unique(X.handle', 'rows')')));

% model manipulation

%% add block to system (scalar, plain block spec)
X = slQuery.gcs ./ 'Gain';
fix = onCleanup(@() delete_block(double(X)));
assert(~isempty(X));
assert(strcmp(X.BlockType, 'Gain'));
assert(X.Parent.wrap == slQuery.gcs);

%% add block to system (scalar, scalar block template)
T = slQuery.gcb;
X = slQuery.gcs ./ T;
fix = onCleanup(@() delete_block(double(X)));
assert(~isempty(X));
assert(strcmp(X.BlockType, T.BlockType));
assert(X.Parent.wrap == slQuery.gcs);

%% add block to system (scalar, block template vector)
T = slQuery('Terminator');
X = slQuery.gcs ./ T;
fix = onCleanup(@() delete_block(double(X)));
assert(all(size(X) == size(T)));
assert(all(strcmp(X.BlockType, T.BlockType)));
assert(all(X.Parent.wrap == slQuery.gcs));

%% add block to system (vector, plain block spec)
S = slQuery('SubSystem[LinkStatus=none]');
X = S ./ 'Gain';
fix = onCleanup(@() delete_block(double(X)));
assert(numel(X) == numel(S));
assert(all(strcmp(X.BlockType, 'Gain')));
assert(all(X.Parent.wrap == S));

%% add block to system (vector, matching block template vector)
[S, T] = slQuery('SubSystem / Terminator')';
X = S ./ T;
fix = onCleanup(@() delete_block(double(X)));
assert(numel(X) == numel(S));
assert(all(strcmp(X.BlockType, T.BlockType)));
assert(all(X.Parent.wrap == S));
