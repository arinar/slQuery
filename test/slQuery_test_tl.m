% test TargetLink features of slQuery
% (run with matlab testrunner: `>> runtests slQuery_test_tl`)

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

%% get block parameter (tlprop, cgdata struct)
x = slQuery(tlgain);
assert(isequaln(x.tl, tl_get(tlgain, 'blockdatastruct')));

%% get block parameter (tlprop, cgdata substruct)
x = slQuery(tlgain);
assert(isequaln(x.tl.output, getfield(tl_get(tlgain, 'blockdatastruct'), 'output')));

%% get block parameter (tlprop, cgdata leaf property)
x = slQuery(tlgain);
assert(isequal(x.tl.output.variable, tl_get(tlgain, 'output.variable')));

%% set block parameter (tlprop, cgdata struct)
fix = fix_param(tlgain, 'data'); %#ok<NASGU> dtor variable
x = slQuery(tlgain);
s = tl_get(tlgain, 'blockdatastruct'); s.output.type = 'Int32';
x.tl = s;
assert(isequal(tl_get(tlgain, 'output.type'), 'Int32'));

%% set block parameter (tlprop, cgdata substruct)
fix = fix_param(tlgain, 'data'); %#ok<NASGU> dtor variable
x = slQuery(tlgain);
s = tl_get(tlgain, 'blockdatastruct'); s.output.type = 'Int32';
x.tl.output = s.output;
assert(isequal(tl_get(tlgain, 'output.type'), 'Int32'));

%% set block parameter (tlprop, cgdata leaf property)
fix = fix_param(tlgain, 'data'); %#ok<NASGU> dtor variable
x = slQuery(tlgain);
x.tl.output.type = 'Int32';
assert(isequal(tl_get(tlgain, 'output.type'), 'Int32'));

%% get block parameter (tlprop, cgdata struct, nonscalar selection)
x = slQuery(tlprods);
assert(isequaln(x.tl, tl_get(tlprods, 'blockdatastruct')));

%% get block parameter (tlprop, cgdata leaf property, nonscalar selection)
x = slQuery(tlprods);
assert(isequal(x.tl.output.variable, tl_get(tlprods, 'output.variable')));

%% set block parameter (tlprop, cgdata struct, nonscalar selection)
fix = fix_param(tlprods, 'data'); %#ok<NASGU> dtor variable
x = slQuery(tlprods);
s = tl_get(tlprods, 'blockdatastruct');
for i = 1:numel(s), s{i}.output.type = 'Int32'; end
x.tl = s;
assert(isequal(tl_get(tlprods, 'output.type'), repmat({'Int32'}, size(tlprods))));

%% set block parameter (tlprop, cgdata leaf property, nonscalar selection)
fix = fix_param(tlprods, 'data');
x = slQuery(tlprods);
x.tl.output.type = 'Int32';
assert(isequal(tl_get(tlprods, 'output.type'), repmat({'Int32'}, size(tlprods))));

% selectors

%% param spec selector (tlprop)
X = slQuery('[tl.output.type=Int16]');
assert(~isempty(X));
assert(all(strcmp(X.tl.output.type, 'Int16')));

%% param spec selector (tlprop, start)
X = slQuery('[tl.output.type^=Float]');
assert(~isempty(X));
assert(all(~cellfun(@isempty, regexp(X.tl.output.type, '^Float', 'once') )));

%% param spec selector (tlprop, end)
X = slQuery('[tl.output.type$=32]');
assert(~isempty(X));
assert(all(~cellfun(@isempty, regexp(X.tl.output.type, '32$', 'once') )));

%% param spec selector (tlprop, part)
X = slQuery('[tl.output.type*=Int]');
assert(~isempty(X));
assert(all(~cellfun(@isempty, regexp(X.tl.output.type, 'Int', 'once') )));

%% param spec selector (tlprop, regex)
X = slQuery('[tl.output.type~=^U?Int(8|32)$]');
assert(~isempty(X));
assert(all(~cellfun(@isempty, regexp(X.tl.output.type, '^U?Int(8|32)$', 'once') )));

%% param spec selector (tlprop, restrict on property exist)
X = slQuery('[tl.gain.type=Int16]');
assert(~isempty(X));
assert(all(strcmp(X.BlockType, 'Gain') & strcmp(X.MaskType, 'TL_Gain')));
assert(all(strcmp(X.tl.output.type, 'Int16')));

%% param spec selector (tlprop, multiple)
X = slQuery('[tl.output.type=Int16,tl.gain.type=Int16]');
assert(~isempty(X));
assert(all(strcmp(X.tl.output.type, 'Int16') & strcmp(X.tl.gain.type, 'Int16')));

%% param spec selector (mixed slparam tlprop)
X = slQuery('[Gain=42,tl.gain.type=Int16]');
assert(~isempty(X));
assert(all(strcmp(X.Gain, '42') & strcmp(X.tl.gain.type, 'Int16')));
