% test features of slQuery
% (run with matlab testrunner: `>> runtests slQuery_test`)

% preconditions
% open test model in fresh state
if ~bdIsLoaded('slQuery_testrefmodel_signal')
	load_system('slQuery_testrefmodel_signal');
	refmodel_signal_dtor = onCleanup(@() close_system('slQuery_testrefmodel_signal', false));
end

if ~bdIsLoaded('slQuery_testrefmodel_physical')
	load_system('slQuery_testrefmodel_physical');
	refmodel_physical_dTor = onCleanup(@() close_system('slQuery_testrefmodel_physical', false));
end

if ~bdIsLoaded('slQuery_testmodel')
	open_system('slQuery_testmodel');
	model_dtor = onCleanup(@() close_system('slQuery_testmodel', false));
end

% special elements
subs2i2o = get_param('slQuery_testmodel/GotoTagVisibilityInside', 'Handle'); % block chosen to have 2 inports, 2 outports

linked = cellfun(@(p) get_param(p, 'Handle'), strcat('slQuery_testmodel/Sort', {'2', '4', '8'})); % blocks chosen to be library linked blocks
children = cellfun(@(p) get_param(p, 'Handle'), strcat('slQuery_testmodel/Signal', {'', 'Split', 'Copy'}, 'Inside/In', {'2', '3', '4'})); % blocks chosen to have a parent block

tlgain = get_param('slQuery_testmodel/Gain', 'Handle'); % block with TargetLink block mask
tlprods = find_system(bdroot(tlgain), 'MaskType', 'TL_Product')'; % uniform set of blocks TargetLink block mask

% block matrices
blk13 = cellfun(@(p) get_param(p, 'Handle'), strcat('slQuery_testmodel/In', {'1', '2', '3'})); % 1x3
blk23 = cellfun(@(p) get_param(p, 'Handle'), strcat('slQuery_testmodel/In', {'1', '2', '3'; '4', '5', '6'})); % 2x3

arr_param = @(arr, p) arrayfun(@(h) {get_param(h, p)}, arr);

% parameter/attribute access

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
X = slQuery(blk13);
assert(isequal(X.Handle, blk13));

%% get block full name (scalar selection)
x = slQuery.gcb;
assert(isequal(x.fullname, gcb));

%% get block full name (nonscalar selection)
X = slQuery(blk23);
assert(isequal(X.fullname, getfullname(blk23)));

%% get block hyperlink (scalar selection)
x = slQuery.gcb;
link = x.hyperlink;
assert(regexp(link, '^<a[^>]+>[^<]+</a>$', 'once') == 1);
assert(str2double(regexp(link, '(?<=hilite_system\()\d+.\d+(?=\))', 'match', 'once')) == gcbh);

%% get block hyperlink (nonscalar selection)
X = slQuery(blk23);
links = X.hyperlink;
assert(isequal(regexp(links, '^<a[^>]+>[^<]+</a>$', 'once'), {1,1,1;1,1,1}));
assert(isequal(str2double(regexp(links, '(?<=hilite_system\()\d+.\d+(?=\))', 'match', 'once')), blk23));

%% get block hyperlink (scalar, name)
x = slQuery.gcb;
link = x.hyperlink('blabliblu');
assert(regexp(link, '^<a[^>]+>blabliblu</a>$', 'once') == 1);

%% get block hyperlink (nonscalar, uniform name)
X = slQuery(blk23);
links = X.hyperlink('blabliblu');
assert(isequal(regexp(links, '^<a[^>]+>blabliblu</a>$', 'once'),  {1,1,1;1,1,1}));

%% get block hyperlink (nonscalar, uniform name)
X = slQuery(blk23);
links = X.hyperlink({'a', 'b', 'c'; 'd', 'e', 'f'});
assert(isequal(regexp(links, '^<a[^>]+>[a-f]</a>$', 'once'),  {1,1,1;1,1,1}));

%% wrap blockref into object (scalar)
x = slQuery.gcb;
assert(isa(x.Parent.wrap, 'slQuery'));

%% wrap blockref into object (nonscalar)
X = slQuery(blk23);
assert(isa(X.Parent.wrap, 'slQuery'));

%% wrap handle into object (scalar)
x = slQuery.gcb;
assert(isa(x.Parent.Handle.wrap, 'slQuery'));

%% wrap handle into object (nonscalar)
X = slQuery(blk23);
assert(isa(X.Parent.Handle.wrap, 'slQuery'));

%% block object parameters act as the properties
x = slQuery.gcb;
assert(isempty(setxor(properties(x), fieldnames(get_param(gcbh, 'ObjectParameters')))));

%% get block parameter (string, scalar)
x = slQuery.gcb;
assert(isequal(x.BackgroundColor, get_param(gcb, 'BackgroundColor')));

%% get block parameter (string, vector)
X = slQuery(blk13);
assert(isequal(X.BackgroundColor, arr_param(blk13, 'BackGroundColor')))

%% get block parameter (string, matrix)
X = slQuery(blk23);
assert(isequal(X.BackgroundColor, arr_param(blk23, 'BackGroundColor')));

%% set block parameter (string, scalar)
x = slQuery.gcb;
fix = fix_param(gcb, 'BackgroundColor'); %#ok<NASGU> dtor variable
x.BackgroundColor = 'black';
assert(isequal(get_param(gcb, 'BackgroundColor'), 'black'));

%% set block parameter (string, vector, from scalar)
x = slQuery(blk13);
fix = fix_param(blk13, 'BackgroundColor'); %#ok<NASGU> dtor variable
x.BackgroundColor = 'black';
assert(isequal(arr_param(blk13, 'BackgroundColor'), repmat({'black'}, 1, 3)));

%% set block parameter (string, vector, from vector)
x = slQuery(blk13);
fix = fix_param(blk13, 'BackgroundColor'); %#ok<NASGU> dtor variable
x.BackgroundColor = {'red', 'yellow', 'green'};
assert(isequal(arr_param(blk13, 'BackgroundColor'), {'red', 'yellow', 'green'}));

%% set block parameter (string, matrix, from scalar)
x = slQuery(blk23);
fix = fix_param(blk23, 'BackgroundColor'); %#ok<NASGU> dtor variable
x.BackgroundColor = 'black';
assert(isequal(arr_param(blk23, 'BackgroundColor'), repmat({'black'}, 2, 3)));

%% set block parameter (string, matrix, from row vector)
x = slQuery(blk23);
fix = fix_param(blk23, 'BackgroundColor'); %#ok<NASGU> dtor variable
x.BackgroundColor = {'red', 'yellow', 'green'};
assert(isequal(arr_param(blk23, 'BackgroundColor'), repmat({'red', 'yellow', 'green'}, 2, 1)));

%% set block parameter (string, matrix, from col vector)
x = slQuery(blk23);
fix = fix_param(blk23, 'BackgroundColor'); %#ok<NASGU> dtor variable
x.BackgroundColor = {'red'; 'green'};
assert(isequal(arr_param(blk23, 'BackgroundColor'), repmat({'red'; 'green'}, 1, 3)));

%% set block parameter (string, matrix, from matrix)
x = slQuery(blk23);
fix = fix_param(blk23, 'BackgroundColor'); %#ok<NASGU> dtor variable
x.BackgroundColor = {'red', 'yellow', 'green'; 'magenta', 'orange', 'cyan'};
assert(isequal(arr_param(blk23, 'BackgroundColor'), {'red', 'yellow', 'green'; 'magenta', 'orange', 'cyan'}));

%% get block parameter (scalar numeric, scalar)
x = slQuery.gcb;
assert(isequal(x.FontSize, get_param(gcb, 'FontSize')));

%% set block parameter (scalar numeric, scalar)
x = slQuery.gcb;
fix = fix_param(gcb, 'FontSize'); %#ok<NASGU> dtor variable
x.FontSize = 23;
assert(isequal(get_param(gcb, 'FontSize'), 23));

%% get block parameter (rectangle, scalar)
x = slQuery.gcb;
assert(isequal(x.Position, get_param(gcb, 'Position')));

%% set block parameter (rectangle, scalar)
x = slQuery.gcb;
fix = fix_param(gcb, 'Position'); %#ok<NASGU> dtor variable
x.Position = (1:4)*100;
assert(isequal(get_param(gcb, 'Position'), (1:4)*100));

%% set block parameter (rectangle, vector, from cell-scalar)
x = slQuery(blk13);
fix = fix_param(blk13, 'Position'); %#ok<NASGU> dtor variable
x.Position = { (1:4)*100 };
assert(isequal(arr_param(blk13, 'Position'), repmat({ (1:4)*100 }, 1, 3)));

%% set block parameter (rectangle, vector, from cell-vector)
x = slQuery(blk13);
fix = fix_param(blk13, 'Position'); %#ok<NASGU> dtor variable
x.Position = { (1:4)*100 (5:8)*100 (9:12)*100 };
assert(isequal(arr_param(blk13, 'Position'), { (1:4)*100 (5:8)*100 (9:12)*100 }));

%% get block parameter (structure, scalar)
x = slQuery(subs2i2o);
assert(isequal(x.LineHandles, get_param(subs2i2o, 'LineHandles')));

% %% set block parameter (structure, scalar)
% TODO: which struct to use?
%x = slQuery(subs2i2o);
%fix = fix_param(subs2i2o, ''); %#ok<NASGU> dtor variable
%assert(false);

%% get block parameter (field of a structure)
x = slQuery(subs2i2o);
assert(isequal(x.LineHandles.Inport, getfield(get_param(subs2i2o, 'LineHandles'), 'Inport')));

% %% set block parameter (field of a structure)
%TODO: which struct to use?
%x = slQuery(subs2i2o);
%fix = fix_param(subs2i2o, ''); %#ok<NASGU> dtor variable
%assert(false);

%% get block parameter (structure-arrayfield-element, scalar)
x = slQuery(subs2i2o);
assert(isequal(x.LineHandles.Inport(2), getfield(get_param(subs2i2o, 'LineHandles'), 'Inport', {2})));

% %% set block parameter (structure-arrayfield-element, scalar)
% TODO: which struct to use?
%x = slQuery(subs2i2o);
%fix = fix_param(subs2i2o, ''); %#ok<NASGU> dtor variable
%assert(false);

%% get block parameter (indirect, string scalar)
x = slQuery(children(1));
assert(isequal(x.Parent.BackgroundColor ...
	, get_param(get_param(children(1), 'Parent'), 'BackgroundColor')));

%% get block parameter (indirect, rectangle scalar)
x = slQuery(children(1));
assert(isequal(x.Parent.Position ...
	, get_param(get_param(children(1), 'Parent'), 'Position')));

%% get block parameter (indirect, string nonscalar)
x = slQuery(children);
assert(isequal(x.Parent.BackgroundColor ...
	, get_param(arr_param(children, 'Parent'), 'BackgroundColor')'));

%% get block parameter (indirect, rectangle nonscalar)
X = slQuery(children);
assert(isequal(X.Parent.Position ...
	, get_param(arr_param(children, 'Parent'), 'Position')'))

%% set block parameter (indirect, string scalar)
x = slQuery(children(1));
fix = fix_param(get_param(children(1), 'Parent'), 'BackgroundColor'); %#ok<NASGU> dtor variable
x.Parent.BackgroundColor = 'purple';
assert(isequal(get_param(get_param(children(1), 'Parent'), 'BackgroundColor'), 'purple'));

%% set block parameter (indirect, rectangle scalar)
x = slQuery(children(1));
fix = fix_param(get_param(children(1), 'Parent'), 'Position'); %#ok<NASGU> dtor variable
x.Parent.Position = [100, 200, 300, 400];
assert(isequal(get_param(get_param(children(1), 'Parent'), 'Position') ...
	, [100, 200, 300, 400]));

%% set block parameter (indirect, vector, from scalar)
X = slQuery(children);
fix = fix_param(get_param(children, 'Parent'), 'Position'); %#ok<NASGU> dtor variable
X.Parent.Position = [100, 200, 300, 400];
assert(isequal(get_param(arr_param(children, 'Parent'), 'Position')' ...
	, repmat({[100, 200, 300, 400]}, 1, numel(children)) ));

% selectors

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
assert(all(~cellfun(@isempty, regexp(X.Name, '^In1', 'match', 'once'))));

%% param spec selector (end)
X = slQuery('[Name$=11]');
assert(~isempty(X));
assert(all(~cellfun(@isempty, regexp(X.Name, '11$', 'match', 'once'))));

%% param spec selector (part)
X = slQuery('[Name*=sig]');
assert(~isempty(X));
assert(all(~cellfun(@isempty, regexp(X.Name, 'sig', 'match', 'once'))));

%% param spec selector (regex)
X = slQuery('[BlockType~=^(In|Out)port$]');
assert(~isempty(X));
assert(all(ismember(X.BlockType, {'Inport', 'Outport'})));

%% param spec selector (multiple params)
X = slQuery('[BlockType=Inport,BackgroundColor=orange]');
assert(~isempty(X));
assert(all(strcmp(X.BlockType, 'Inport')));
assert(all(strcmp(X.BackgroundColor, 'orange')));

%% argidx selectors (single path)
x = slQuery('(1)', gcb);
assert(x == gcbh);

%% argidx selectors (single handle)
x = slQuery('(1)', gcbh);
assert(x == gcbh);

%% argidx selectors (pathes candidate list)
x = slQuery('(1)', {gcb}); % all these blocks must exist TODO: it'd be better if they hadn't have to
assert(x == gcbh);

%% argidx selectors (handle candidate list)
x = slQuery('(1)', [gcbh, -0.42, -0.23]); % nonexisting handles won't be matched
assert(x == gcbh);

%% combined selectors (wildcard and param spec)
X = slQuery('*[Name^=Subsystem]');
assert(~isempty(X));
assert(all(~cellfun(@isempty, regexp(X.Name, '^Subsystem', 'match', 'once'))));

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

% block port and line handle syntax using colon

%% port handle access (scalar, single inport)
for x = slQuery('Mux')
	assert(isa(1:x, 'slQuery'));
	assert(x.PortHandles.Inport(1) == 1:x);
	assert(x.PortHandles.Inport(2) == 2:x);
end

%% port handle access (scalar, single outport)
for x = slQuery('Demux')
	assert(x.PortHandles.Outport(1) == x:1);
	assert(x.PortHandles.Outport(2) == x:2);
end

%% port handle access (scalar, single conn)
for x = slQuery('SimscapeBus')
	assert(x.PortHandles.LConn(1) == 1j:x);
	assert(x.PortHandles.LConn(2) == 2j:x);
	assert(x.PortHandles.RConn(1) == x:1j);
end

%% port handle access (scalar, reset)
for x = slQuery('SubSystem[Name^=Resettable]')
	assert(x.PortHandles.Reset == '�':x)
end

%% port handle access (scalar, enable)
for x = slQuery('SubSystem[Name^=Enabled]')
	assert(x.PortHandles.Enable == '?':x)
end

%% port handle access (scalar, trigger)
for x = slQuery('SubSystem[Name^=Triggered]')
	assert(x.PortHandles.Trigger == '!':x)
end

%% port handle access (scalar, ifaction)
for x = slQuery('SubSystem[Name^=Action]')
	assert(x.PortHandles.Ifaction == '%':x)
end

%% port handle access (scalar, state)
for x = slQuery('[Name^=StatePortHaving]')
	assert(x.PortHandles.State == x:'^')
end

%% port handle access (scalar, single inport line)
for x = slQuery('Mux')
	assert(x.LineHandles.Inport(1) == -1:x);
	assert(x.LineHandles.Inport(2) == -2:x);
end

%% port handle access (scalar, single outport line)
for x = slQuery('Demux')
	assert(x.LineHandles.Outport(1) == x:-1);
	assert(x.LineHandles.Outport(2) == x:-2);
end

%% port handle access (scalar, reset line)
for x = slQuery('SubSystem[Name^=Resettable]')
	assert(x.LineHandles.Reset == '-�':x)
end

%% port handle access (scalar, enable line)
for x = slQuery('SubSystem[Name^=Enabled]')
	assert(x.LineHandles.Enable == '-?':x)
end

%% port handle access (scalar, trigger line)
for x = slQuery('SubSystem[Name^=Triggered]')
	assert(x.LineHandles.Trigger == '-!':x)
end

%% port handle access (scalar, ifaction line)
for x = slQuery('SubSystem[Name^=Action]')
	assert(x.LineHandles.Ifaction == '-%':x)
end

%% port handle access (scalar, state line)
for x = slQuery('[Name^=StatePortHaving]')
	assert(x.LineHandles.State == x:'-^')
end

%% port handle access (scalar, single conn line)
for x = slQuery('SimscapeBus')
	assert(x.LineHandles.LConn(1) == -1j:x);
	assert(x.LineHandles.LConn(2) == -2j:x);
	assert(x.LineHandles.RConn(1) == x:-1j);
end

%% port handle access (nonscalar, single port)
X = slQuery('Mux');
assert(all(X.PortHandles.Inport(2) == 2:X));
assert(all(X.PortHandles.Outport(1) == X:1));

%% port handle access (nonscalar, single conn)
X = slQuery('SimscapeBus');
assert(all(X.PortHandles.LConn(2) == 2j:X));
assert(all(X.PortHandles.RConn(1) == X:1j));

%% port handle access (nonscalar, single port line)
X = slQuery('Demux');
assert(all(X.LineHandles.Inport(1) == -1:X));
assert(all(X.LineHandles.Outport(2) == X:-2));

%% port handle access (nonscalar, single conn line)
X = slQuery('SimscapeBus');
assert(all(X.LineHandles.LConn(2) == -2j:X));
assert(all(X.LineHandles.RConn(1) == X:-1j));

%% port handle access (scalar, multiple inports)
for x = slQuery('Mux')
	assert(all([x.PortHandles.Inport(1); x.PortHandles.Inport(2)] == [1 2]:x));
	assert(all([x.LineHandles.Inport(1); x.LineHandles.Inport(2)] == -[1 2]:x));
end

%% port handle access (scalar, multiple outports)
for x = slQuery('Demux')
	assert(all([x.PortHandles.Outport(1); x.PortHandles.Outport(2)] == x:[1 2]));
	assert(all([x.LineHandles.Outport(1); x.LineHandles.Outport(2)] == x:-[1 2]));
end

%% port handle access (scalar, multiple conn)
for x = slQuery('SimscapeBus')
	assert(all([x.PortHandles.LConn(1); x.PortHandles.LConn(2)] == [1j 2j]:x));
	assert(all([x.LineHandles.LConn(1); x.LineHandles.LConn(2)] == -[1j 2j]:x));
	% must use the same port number, because there aren't any dual-rconn blocks in testmodel
	assert(all([x.PortHandles.RConn(1); x.PortHandles.RConn(1)] == x:[1j 1j]));
	assert(all([x.LineHandles.RConn(1); x.LineHandles.RConn(1)] == x:-[1j 1j]));
end

%% port handle access (scalar, mixed ports and lines)
for x = slQuery('Demux')
	assert(all([x.LineHandles.Inport(1); x.PortHandles.Outport(2)] == -1:x:2));
	assert(all([x.LineHandles.Inport(1); x.PortHandles.Inport(1); x.PortHandles.Outport(1); x.LineHandles.Outport(2)] == [-1 1]:x:[1 -2]));
end

%% port handle access (scalar, mixed conn and line)
for x = slQuery('SimscapeBus')
	assert(all([x.LineHandles.LConn(1); x.PortHandles.RConn(1)] == -1j:x:1j));
	assert(all([x.LineHandles.LConn(1); x.PortHandles.LConn(1); x.PortHandles.RConn(1); x.LineHandles.RConn(1)] == [-1j 1j]:x:[1j -1j]));
end

%% port handle access (nonscalar, multiple inports)
X = slQuery('Mux');
assert(all(all([X.PortHandles.Inport(1); X.PortHandles.Inport(2)] == [1 2]:X)));

%% port handle access (nonscalar, multiple outports)
X = slQuery('Mux');
assert(all(all([X.PortHandles.Inport(1); X.PortHandles.Inport(2)] == [1 2]:X)));

%% port handle access (nonscalar, multiple conn)
X = slQuery('SimscapeBus');
assert(all(all([X.PortHandles.LConn(1); X.PortHandles.LConn(2)] == [1j 2j]:X)));

%% port handle access (nonscalar, mixed ports and lines)
X = slQuery('Demux');
assert(all(all([X.LineHandles.Inport(1); X.PortHandles.Outport(2)] == -1:X:2)));
assert(all(all([X.LineHandles.Inport(1); X.PortHandles.Inport(1); X.PortHandles.Outport(1); X.LineHandles.Outport(2)] == [-1 1]:X:[1 -2])));

%% port handle access (nonscalar, mixed conn and lines)
X = slQuery('SimscapeBus');
assert(all(all([X.LineHandles.LConn(1); X.PortHandles.RConn(1)] == -1j:X:1j)));
assert(all(all([X.LineHandles.LConn(1); X.PortHandles.LConn(1); X.PortHandles.RConn(1); X.LineHandles.RConn(1)] == [-1j 1j]:X:[1j -1j])));

% back references with $i and ,-combinator

%% join combinator
assert(~isempty(slQuery('*, *')));

%% backref selector does not yield a column
x = slQuery('SubSystem, $1');
assert(size(x, 1) == 1);

%% backref selector, more uses
x = slQuery('SubSystem, $1, Terminator, $2, $1');
assert(size(x, 1) == 2);

%% backref in param spec
X = slQuery('Mux, Demux[BackgroundColor=$1.BackgroundColor]');
assert(all(cellfun(@strcmp, X(1).BackgroundColor, X(2).BackgroundColor)));

%% backref in param spec (multiple backrefs)
X = slQuery('Mux, Demux[BackgroundColor=$1.BackgroundColor,ForegroundColor=$1.ForegroundColor]');
assert(all(cellfun(@strcmp, X(1).BackgroundColor, X(2).BackgroundColor)));

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
[G,F] = slQuery('Goto, From')'; %#ok<RHSFN> it can do this
assert(size(G, 1) == 1);
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
fix = fix_param(double(X), 'BackgroundColor'); %#ok<NASGU> dtor variable
X([1 2]).BackgroundColor = {'green'; 'red'};

%% numeric indexing for columns (set parameter, distribute row)
X = slQuery('Goto, From');
fix = fix_param(double(X), 'BackgroundColor'); %#ok<NASGU> dtor variable
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

% hierarchy combinators
%% child combinator
X = slQuery('SubSystem / Goto');
assert(~isempty(X));
for x = X
	assert(x(1).Handle == x(2).Parent.Handle);
end

%% parent combinator
X = slQuery('Goto \ SubSystem');
assert(all(X(1).Parent.Handle == X(2).Handle));

%% child and parent yield same info
X = slQuery('SubSystem / Goto \ SubSystem');
assert(isequal(X(1), X(3)));

%% descendents combinator
for x = slQuery('SubSystem // Goto')
	s = x(2);
	while s ~= 0
		s = s.Parent.Handle.wrap;
		if s == x(1), break, end
	end
	assert(s ~= 0);
end

%% ascendants combinator
[B, S] = slQuery('Goto \\ SubSystem')'; %#ok<RHSFN> it can do this
assert(~isempty(B));
while any(S) % iteratively set Bs to their parents drop them when they match S
	B = B.Parent.Handle.wrap;
	i = B == S; % drop all blocks that match up
	B = B(~i);
	S = S(~i);
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

%% physical line combinator (indirectional)
for x = slQuery('PMIOPort[Side=Left,BackgroundColor=magenta] - PMIOPort[Side=Right,BackgroundColor=cyan]')
	% naive: there are no splits - same line
	if x(1):-1j == x(2):-1j, continue, end %#ok<*BDSCA> it can do this
	
	% slightly less naive: at least one line is in the others LineChildren
	if any(x(1):-1j == getfield(x(2):-1j, 'LineChildren')), continue, end %#ok<GFLD> i want `(x(2):-1j).LineChildren` syntax
	if any(x(2):-1j == getfield(x(1):-1j, 'LineChildren')), continue, end %#ok<GFLD>
	
	% it might be a special case where two separately grown trees collide
	assert(false);
end

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

%% physical line combinator with portspec (rconn number)
for x = slQuery('SubSystem:1 - PMIOPort')
	% naive: there are no splits - same line
%  	disp(x.hyperlink')
% 	disp(x(1):-1j == x(2):-1j);
% 	assert(x(1):-1j == x(2):-1j);
end

%% physical line combinator with portspec (lconn number)
for x = slQuery('PMIOPort - 1:SubSystem')
	% naive: there are no splits - same line
% 	disp(x.hyperlink')
%  	disp(x(1):-1j == x(2):-1j);
% 	assert(x(1):-1j == x(2):-1j);
end

%% signal line combinators with portspec (port name, inport)
for x = slQuery('Inport -> In2: SubSystem :In2 <- Inport')
	i = str2double(get_param([x(2).fullname '/In2'], 'Port'));
	assert(x(1):1 == get_param(double(-(i):x(2)), 'SrcPortHandle'));
	assert(x(1) == x(3));
end

%% signal line combinators with portspec (port name, outport)
for x = slQuery('Outport <- Out2: SubSystem :Out2 -> Outport')
	i = str2double(get_param([x(2).fullname '/Out2'], 'Port'));
	assert(x(2):(i) == get_param(double(-1:x(1)), 'SrcPortHandle'));
	assert(x(1) == x(3));
end

%% physical line combinator with portspec (conn name)
for x = slQuery('PMIOPort - LConn2: SubSystem :RConn2 - PMIOPort')
	i = str2double(get_param([x(2).fullname '/LConn2'], 'Port'));
	assert(x(2):(i) == get_param(double(-1:x(1)), 'SrcPortHandle'));
	assert(x(1):-1j == x(2):-1j);
end

%% signal line combinators with portspec (enable port)
for x = slQuery('Ground -> ?:*')
	assert(x(1).LineHandles.Outport == x(2).LineHandles.Enable);
end

%% signal line combinators with portspec (enable port, reverse)
for x = slQuery('*:? <- Ground')
	assert(x(2).LineHandles.Outport == x(1).LineHandles.Enable);
end

%% signal line combinators with portspec (trigger port)
for x = slQuery('Ground -> !:*')
	assert(x(1).LineHandles.Outport == x(2).LineHandles.Trigger);
end

%% signal line combinators with portspec (trigger port, reverse)
for x = slQuery('*:! <- Ground')
	assert(x(2).LineHandles.Outport == x(1).LineHandles.Trigger);
end

%% signal line combinators with portspec (action port)
for x = slQuery('If -> %:*')
	assert(any(x(1).LineHandles.Outport == x(2).LineHandles.Ifaction));
end

%% signal line combinators with portspec (action port, reverse)
for x = slQuery('*:% <- If')
	assert(any(x(2).LineHandles.Outport == x(1).LineHandles.Ifaction));
end

%% signal line combinators with portspec (reset port)

for x = slQuery('Ground -> �:*')
	assert(x(1).LineHandles.Outport == x(2).LineHandles.Reset);
end

%% signal line combinators with portspec (reset port, reverse)
for x = slQuery('*:� <- Ground')
	assert(x(2).LineHandles.Outport == x(1).LineHandles.Reset);
end

%% signal line combinators with portspec (state port)
for x = slQuery('*:^ -> Terminator')
	assert(x(2).LineHandles.Inport == x(2).LineHandles.State);
end

%% signal line combinators with portspec (state port, reverse)
for x = slQuery('Terminator <- ^:*')
	assert(x(1).LineHandles.Inport == x(1).LineHandles.State);
end

%% downstream and upstream signal line combinators yield same info
for x = slQuery('Inport -> Outport <- Inport')
	assert(x(1) == x(3));
end

% dataflow wiring combinators - colored port blocks are set up in the test model so the port
% numbers and the numbers in their block names match the data flow correspondence

%% wiring combinator (downstream)
[i, o] = slQuery('Inport[BackgroundColor=orange] => Outport[BackgroundColor=lightBlue]')'; %#ok<RHSFN> it can do this
assert(~isempty(i));
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(i.Name, '\d+', 'match', 'once'))));
assert(all(strcmp(i.Port, o.Port)));

%% wiring combinator (upstream)
[o, i] = slQuery('Outport[BackgroundColor=lightBlue] <= Inport[BackgroundColor=orange]')'; %#ok<RHSFN> it can do this
assert(~isempty(o));
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(i.Name, '\d+', 'match', 'once'))));
assert(all(strcmp(i.Port, o.Port)));

% TODO: physical wiring combinator (connection)
%for x = slQuery('Mux = Mux')
%end

% a few extra ports along the wires are places in subsystems, they are also named correctly

%% wiring combinator (downstream, including virtual blocks)
[i, o] = slQuery('Inport[BackgroundColor=orange] ~> Outport[IsBusElementPort=off]')'; %#ok<RHSFN> it can do this
assert(~isempty(i));
assert(any(strcmp(o.BackgroundColor, 'white')));
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(o.Name, '\d+', 'match', 'once'))));

%% wiring combinator (upstream, including virtual blocks)
[o, i] = slQuery('Outport[BackgroundColor=lightBlue] <~ Inport[IsBusElementPort=off]')'; %#ok<RHSFN> it can do this
assert(~isempty(o));
assert(any(strcmp(i.BackgroundColor, 'white')));
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(o.Name, '\d+', 'match', 'once'))));

% TODO: physical wiring combinator (including virtual blocks)
%for x = slQuery('Mux ~ Mux')
%end

% signal slicing combinators

% colored slicing subsystems are set up in the test model so the numbers in their blocks names
% match

%% signal slicing combinator (downstream)
[i, o] = slQuery('Inport[BackgroundColor=green] >> Outport[BackgroundColor=red]')'; %#ok<RHSFN> it can do this
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(o.Name, '\d+', 'match', 'once'))));

%% signal slicing combinator (upstream)
[o, i] = slQuery('Outport[BackgroundColor=red] << Inport[BackgroundColor=green]')'; %#ok<RHSFN> it can do this
assert(all(strcmp(regexp(i.Name, '\d+', 'match', 'once'), regexp(o.Name, '\d+', 'match', 'once'))));

% TODO: physical slicing combinator (indirectional)
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

%% resolved link to a library block
[blk, lib] = slQuery('SubSystem -@ *')'; %#ok<RHSFN> it can do this
assert(all(strcmp(blk.ReferenceBlock, lib.fullname)));

%% all active links to a block
[~, lib, blk] = slQuery('(1) // * @- SubSystem', {'slQuery_testlibrary'})'; %#ok<RHSFN> it can do this
assert(all(strcmp(blk.ReferenceBlock, lib.fullname)));

%% all active links to a block (legacy)
X = slQuery('(1) // * @ SubSystem', {'slQuery_testlibrary'});
assert(all(strcmp(X(3).ReferenceBlock, X(2).fullname)));

% combinatorics

%% combine results (common info)
% $2 is handled in groups, because it repeats
X = slQuery('Inport \ #GotoTagVisibilityInside / Outport');
assert(isequal(size(X), size(unique(X.handle', 'rows')')));

% model manipulation

%% add block to system (scalar, plain block spec)
X = slQuery.gcs ./ 'Gain';
fix = onCleanup(@() delete_block(double(X)));
assert(~isempty(X));
assert(strcmp(X.BlockType, 'Gain'));
assert(X.Parent.wrap == slQuery.gcs);

%% add block to system (scalar, block spec with name)
X = slQuery.gcs ./ 'Gain#MyGain';
fix = onCleanup(@() delete_block(double(X)));
assert(~isempty(X));
assert(strcmp(X.BlockType, 'Gain'));
assert(strcmp(X.Name, 'MyGain'));
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
[S, T] = slQuery('SubSystem / Terminator')'; %#ok<RHSFN> it can do this
X = S ./ T;
fix = onCleanup(@() delete_block(double(X)));
assert(numel(X) == numel(S));
assert(all(strcmp(X.BlockType, T.BlockType)));
assert(all(X.Parent.wrap == S));

%% TODO: tests for all empty cases
% empty row vector
% empty col vector
% empty matrix
% empty port

