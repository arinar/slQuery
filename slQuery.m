%{
                               _  ____
                           ___| |/ __ \ _   _  ___ _ __ _   _
                          / __| | |  | | | | |/ _ \ '__| | | |
                          \__ \ | |__| | |_| |  __/ |  | |_| |
                          |___/_|\___\_\\__,_|\___|_|   \__, |
                            easy-as-pie API to Simulink |___/

v1.8, 2020 robert@raschhour.com

slQuery is free software: you can redistribute it and/or modify it under the terms of the GNU
General Public License as published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

slQuery is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along with slQuery. If not,
see <http://www.gnu.org/licenses/>.
%}
classdef slQuery < double
	%SLQUERY  easy-as-pie API to Simulink
	methods
		function this = slQuery(query, varargin)
			%if nargin == 0, return, end; % syntax for allocation
			if isnumeric(query) % simple handle-array conversion
				handles = query;
				
			elseif iscellstr(query) % fullname-array conversion
				handles = cellfun(@(s) get_param(s, 'Handle'), query);
				
			elseif ischar(query) % normal query
				% additional arguments represent sets of blocks to pick from via (123) - index
				idx = cellfun(@isnumeric, varargin);
				varargin(idx) = cellfun(@(hs) {double(reshape(hs, 1, []))}, varargin(idx));
				
				idx = cellfun(@ischar, varargin);
				varargin(idx) = cellfun(@(s) {get_param(s, 'Handle')}, varargin(idx));
				
				idx = cellfun(@iscellstr, varargin); % TODO: also accept block pathes, that don't exist
				varargin(idx) = cellfun(@(cs) {cell2mat(get_param(cs, 'Handle'))'}, varargin(idx));
				
				assert(~isempty(bdroot), 'no simulink diagram is active');
				
				handles = slQuery.select(strtrim(query), varargin{:});
			else
				error('illegal arguments');
			end
			
			this = this@double(handles);
		end
	end
	methods(Access=public, Hidden)
		function disp(this)
			if isempty(this)
				fprintf('   %dx0 empty slQuery\n', size(this, 1));
			else
				disp('slQuery with handles');
				x = dbstack; % hyperlinks don't look nice in datatip displays
				if numel(x) >= 2 && strncmp(x(2).name, 'datatipinfo', 11)
					disp(double(this));
				else
					% must build one big array of text, so that it can all print instantaneously
					mag = floor(log10(max(max(double(this)))));
					if mag < 3, mag = 0; end
					repr = arrayfun(@(h, l) sprintf('% *s<a href="matlab: hilite_system(%.15f);">%1.4f</a>', min(4, 5-ceil(log10(l))), '', h, l), ...
						double(this)', double(this)' / 10.^mag, 'UniformOutput', false);
					if mag > 0
						fprintf('   1.0e+%02d *', mag);
					end
					
					w = get(0, 'CommandWindowSize'); w = floor(w(1) / 10);
					if size(repr, 1) > w
						% must print groups of columns that fit the screen
						for i = 1:w:size(repr, 1)
							j = min(i+w-1, size(repr, 1));
							fprintf('  Columns %d through %d\n', i, j)
							crepr = repr(i:j, :);
							crepr(end+1, 1:end-1) = {char(10)}; %#ok<CHARTEN,AGROW>
							disp([crepr{:}]);
						end
					else % can print evrything in one go.
						repr(end+1, 1:end-1) = {char(10)}; %#ok<CHARTEN>
						disp([char(10) repr{:}]) %#ok<CHARTEN>
					end
				end
			end
		end
		function sel = subsref(sel, subs)
			for sub = subs
				switch sub.type
					case '()' % selection
						if isa(sel, 'slQuery')
							if isscalar(sub.subs) && isnumeric(sub.subs{1}) && ~isvector(sel)
								sub.subs = [sub.subs, ':'];
							elseif isscalar(sub.subs) && islogical(sub.subs{1})
								sub.subs = [':', sub.subs];
							end
							sel = slQuery(subsref(double(sel), sub));
						elseif iscell(sel) % distribute index access across all cells
							sel = cellfun(@(c) {subsref(c, sub)}, sel);
							if all(cellfun(@isscalar, sel)), sel = cell2mat(sel); end
						else
							sel = builtin('subsref', sel, sub);
						end
						
					case '.' % parameter access
						switch lower(sub.subs)
							case {'show', 'showall'} % show the situations column by column
								cs = gcs;
								for col = double(sel)
									hilite_system(col);
									if strcmp(sub.subs, 'showall')
										disp(col); pause(0.5);
									else
										pause
									end
									hilite_system(col, 'none');
								end
								open_system(cs);
							
							case 'hyperlink' % an array of links to the blocks (with their name)
								subs(1:find(strcmp({subs.subs}, 'hyperlink'))) = [];
								if ~isempty(subs) && strcmp(subs(1).type, '()') && iscell(subs(1).subs) ... allow hyperlink subsref
										&& isscalar(subs(1).subs) && (ischar(subs(1).subs{1}) || iscellstr(subs(1).subs{1})) % with one char or cellstr like a fcn-call
									names = subs(1).subs{1};
								else
									names = slQuery.get_param(sel, 'Name');
								end
								sel = slQuery.arrayfun( ...
									@(h, n) { sprintf('<a href="matlab: hilite_system(%.15f);">%s</a>', h, n) } ...
									, double(sel), names);
								return
								
							case 'wrap' % TODO: is this cool?
								if ischar(sel)
									sel = get_param(sel, 'Handle');
								elseif iscellstr(sel)
									sel = cellfun(@(b) get_param(b, 'Handle'), sel);
								end
								sel = slQuery(sel);

							case 'fullname' % return the model path of all blocks
								sel = getfullname(double(sel));
								
							case 'root' % return the block diagram root of all blocks
								sel = bdroot(double(sel));

							case 'tl' % return a TargetLink property
								subs(1:find(strcmp({subs.subs}, 'tl'))) = [];
								sel = arrayfun(@(h) tl_get(h, 'blockdatastruct'), double(sel), 'UniformOutput', false);
								if ~isempty(subs)
									sel = cellfun(@(p) subsref(p, subs), sel, 'UniformOutput', false);
								end
								if isscalar(sel), sel = sel{1}; end
								break

							otherwise % select a block parameter, field of a struct or property of an object
								
								if isstruct(sel) || (isobject(sel) && ~isa(sel, 'slQuery')) || all(all(ishandle(sel))) % field of a struct or property of an object
									sel = arrayfun(@(h) h.(sub.subs), sel, 'UniformOutput', false);
								
								elseif iscell(sel) && all(all(cellfun(@(s) isobject(s) | isstruct(s), sel))) % (hopefully) shared member of collection of different objects
									sel = cellfun(@(h) h.(sub.subs), sel, 'UniformOutput', false);
								
								else % something from simulink
									if isa(sel, 'slQuery') % get_param of a handle
										sel = double(sel);
									elseif ischar(sel) % possibly single block path.
										sel = get_param(sel, 'Handle');
									elseif iscellstr(sel) % possibly multiple block pathes (ReferenceBlock, Ancestor-Block, Parent, ...)
										sel = cellfun(@(h) get_param(h, 'Handle'), sel);
									end
									
									ot = slQuery.get_paramtype(sel, sub.subs);
									sel = arrayfun(@(h) get_param(h, sub.subs), sel, 'UniformOutput', ismember(ot, {'real'}));
								end
						end
				end
				% LEGACY. this isn't actually cool, because it breaks algorhitms that work with entire
				% query-results, when those just happen to be scalars. TODO: need a way to decide this!
				if iscell(sel) && isscalar(sel)
					sel = sel{1};
				end
			end
		end

		function this = subsasgn(this, subs, value)
			% TODO: support "this(select) = []" - syntax for dropping rows or columns
			sel = this;
			for sub = subs
				switch sub.type
					case '()' % slicing
						if isa(sel, 'slQuery')
							if isscalar(sub.subs) && isnumeric(sub.subs{1}) && ~isvector(sel)
								sub.subs = [sub.subs, ':'];
							elseif isscalar(sub.subs) && islogical(sub.subs{1})
								sub.subs = [':', sub.subs];
							end
							sel = slQuery(builtin('subsref', double(sel), sub));
						else
							sel = builtin('subsref', sel, sub);
						end
						
					case '.' % parameter access
						if strcmp(sub.subs, 'tl') % set TargetLink property ~> collect all items up for tl_set
							subs(1:find(strcmp({subs.subs}, 'tl'))) = [];
							if ~isempty(subs)
								tlprops = arrayfun(@(h) tl_get(h, 'blockdatastruct'), double(sel), 'UniformOutput', false);
								tlprops = slQuery.arrayfun(@subsasgn, tlprops, {subs}, value);
							else
								tlprops = slQuery.arrayfun(@(~, v) v, sel, value);
							end
							slQuery.arrayfun(@tl_set, double(sel), 'blockdatastruct', tlprops);
							break; % all further subs were resolved here
						end
						
						switch slQuery.get_paramtype(double(sel), sub.subs)
							case {'rectangle', 'ports'} % homogenous array ~> pack last dimension into cell
								if ~iscell(value)
									value = num2cell(value, numel(size(value)));
									value = cellfun(@(x) {squeeze(x)'}, value);
								end
						end
						
						slQuery.arrayfun(@set_param, double(sel), sub.subs, value);
						
					otherwise % they did something stupid
						error('''%s'' not supported');
				end
			end
		end
	end
	methods(Access=public) % operators that are operations
		function ps = properties(this)
			ps = sort(fieldnames(get_param(double(this), 'ObjectParameters')));
		end
		function new = ldivide(spec, sys)
			new = slQuery.rdivide(sys, spec);
		end
		function new = rdivide(sys, spec) % add blocks to subsystems
			target = getfullname(double(sys));
			% TODO: parse the spec entirely (param-specs, class)
			if ischar(spec) || iscellstr(spec) % it's a block type spec
				spec = regexp(spec, '(?<type>\w+)(#)?(?<name>(?(2)\w+))', 'names', 'once');
				assert(~isempty(spec));
				if isempty(spec.name), spec.name = ['slq_' spec.type]; end
				source = strcat('built-in/', spec.type);
				target = strcat(target, '/', spec.name);
			
			elseif isa(spec, 'slQuery') % it's a list blocks that exist already
				source = double(spec);
				names = get_param(source, 'Name');
				if isrow(names) == iscolumn(target), names = names'; end
				target = strcat(target, '/', names);
			end
			new = slQuery(slQuery.arrayfun(@add_block, source, target, 'MakeNameUnique', 'on'));
		end
		function ps = colon(i, this, o) % retrieve port (or line) hanldes ~> x:1, -1:x
			% TODO: support port names for subsystems
			
			if nargin == 2, o = [];
				if isa(i, 'slQuery'), o = this; this = i; i = []; end % rephrase the case "b:o" case as "[]:b:o"
			end
			assert(isempty(i) || isscalar(i) || isempty(o) || isscalar(o) || numel(i) == numel(o), ... so both sides can be combined
				'input and output port spec cardinality must match (i=%d, o=%d)', numel(i), numel(o));
			
			lhs = slQuery.get_param(double(this), 'LineHandles');
			phs = slQuery.get_param(double(this), 'PortHandles');
			
			if ischar(i)
				if i(1) == '-', hs = lhs; i(1) = []; else, hs = phs; end
				switch i
					case '!', f = 'Trigger';
					case '?', f = 'Enable';
					case '%', f = 'Ifaction';
					case '°', f = 'Reset';
					otherwise, error('left handle index must be real or imaginary integer or one of !, ?, %, °');
				end
				its(:, 1) = arrayfun(@(h) h.(f), hs);
			elseif isreal(i)
				its(:, i>0) = cell2mat(arrayfun(@(h) {h.Inport(i(i>0))}, phs));
				its(:, i<0) = cell2mat(arrayfun(@(h) {h.Inport(-i(i<0))}, lhs));
			else
				i = imag(i);
				its(:, i>0) = cell2mat(arrayfun(@(h) {h.LConn(i(i>0))}, phs));
				its(:, i<0) = cell2mat(arrayfun(@(h) {h.LConn(-i(i<0))}, lhs));
			end
			
			if ischar(o)
				if o(1) == '-', hs = lhs; o(1) = []; else, hs = phs; end
				assert(ismember(o, {'^'}), 'right handle index must be real or imaginary integer or ^');
				ots(:, 1) = arrayfun(@(h) h.State, hs);
			elseif isreal(o)
				ots(:, o>0) = cell2mat(arrayfun(@(h) {h.Outport(o(o>0))}, phs));
				ots(:, o<0) = cell2mat(arrayfun(@(h) {h.Outport(-o(o<0))}, lhs));
			else
				o = imag(o);
				ots(:, o>0) = cell2mat(arrayfun(@(h) {h.RConn(o(o>0))}, phs));
				ots(:, o<0) = cell2mat(arrayfun(@(h) {h.RConn(-o(o<0))}, lhs));
			end
			ps = slQuery([its ots]');
		end
		function ls = minus(sps, dps) % add a line between ports x:1 - 1:y
			sys = slQuery.get_ref(slQuery.get_ref(double(sps), 'Parent'), 'Parent')';
			ls = slQuery.arrayfun(@(dp) get_param(double(dp), 'line'), dps);
			delete_line(ls(ls ~= -1));
			ls = slQuery.arrayfun(@add_line, reshape(sys, 1, []), reshape(double(sps), 1, []), reshape(double(dps), 1, []), 'Autorouting', 'on');
			ls = slQuery(ls);
		end
		function mpower(target, hss) % ~> 'attach to void' []^x
			for hs = double(hss)
				assert(isa(target, 'double') && isempty(target));
				type = unique(slQuery.get_param(hs, 'type'));
				assert(isscalar(type));
				switch type{1}
					case 'line'
						delete_line(hs(hs ~= -1));
					case 'block'
						ls = slQuery.get_param(hs, 'LineHandles');
						delete_line(setdiff([ls.Inport ls.Outport], -1));
						delete_block(hs);
				end
			end
		end
		function [varargout] = ctranspose(this) % allow 'dispersed' assignments: [a, b, ~] = slQuery(...)';
			if nargout < 2, varargout{1} = this; return; end % does not transpose the array
			varargout = cell(1, nargout);
			handles = double(this);
			for i = 1:nargout
				varargout{i} = slQuery(handles(i, :));
			end
		end
	end
	methods(Access=private, Static)
		function handles = select(query, varargin) % core "select" algorithm of slQuery
			% split query along the combinators                                                                                                                           ( outside [] )
			[selectors, combinators] = regexp(query, '\s*( |\\\\|\\|//|/|,|@|§|´|`|(:\s*(\w+|[?!%°^])\s*)?(->|-|<-|~>|~(?!=)|<~|=>|<=|>>|<>|<<)(\s*(\w+|[?!%°^])\s*:)?)\s*(?![^\[]*\])', 'split', 'match');
			
			% start with the search root and combinator ',' for arbitrary position in this root
			hot = get_param(bdroot, 'Handle'); % always search only in current model
			handles = double.empty(0, 1);
			for act = [',' combinators; selectors]
				% parse the combinator:     '    (     colon with portspec     )...(                          combinator type (again)                           )...(      portspec with colon     )
				combinator = regexp(act{1}, '^\s*(:)?(?<sp>(?(1)(\w+|[?!%°^])))?\s*(?<type>( |\\\\|\\|//|/|,|@|§|´|`|->|-|<-|~>|~|<~|=>|<=|>>|<>|<<(?![^\[]*\])))\s*(?<dp>(\w+|[?!%°^]))?\s*(?(4):)?\s*$', 'names');
				
				% cast numeric port qualifiers
				if ~isnan(str2double(combinator.sp)), combinator.sp = str2double(combinator.sp); end
				if ~isnan(str2double(combinator.dp)), combinator.dp = str2double(combinator.dp); end
				
				% build the find_system filter from blockspec (all searches are reduced to this)
				common_find_args = {'FollowLinks', 'on', 'LookUnderMasks', 'all', 'Variants', 'All', 'IncludeCommented', 'on', 'Regexp', 'on'};
				if verLessThan('matlab', '8'), common_find_args(7:8) = []; end
				
				% combinators always represent a search relating to some set of properties from a row of blocks
				% selected previously - the "hot" row. In order to avoid multiple seaches based on the same
				% infos (that would result in the same outcome), we can group hot based on this distinguishing
				% info and perform the search once for each group and then outer-join the result to the original
				% set of columns
				switch combinator.type
					case ',' % group blocks by the block diagram root (only relation they have)
						hinfos = bdroot(hot);
					case {'\', '\\'} % group by parent of the last blocks
						% case {'\', '\\', ' '}  NOTE/TODO: the sibling-combinator ' ' can't be here included here because each block cannot be included amongst its own siblings
						hinfos = slQuery.get_ref(hot, 'Parent');
					case {'§', '´'}
						hinfos = slQuery.get_ref(hot, 'ReferenceBlock');
					otherwise % group only by block-handle itself
						hinfos = hot;
				end
				
				if regexp(act{2}, '^\$\d+$', 'match', 'once') % selector is a reference ~> simple column-number
					selector = str2double(act{2}(2:end));
					new_handles = double.empty(size(handles, 1), 0); % height of result is the same
					
				else % selector is real ~> create structure
					% parse as selector:       ^(*)(    parens around arg index    )(block type )(  hash with name  )( period with masktype )(    brackets and qualifier list   )(  plus and pseudo-class )$
					assert(~isempty(act{2}), 'missing selector after ''%s''', act{1});
					selector = regexp(act{2}, '^\*?(\()?(?<argidx>(?(1)\d+))(?(1)\))(?<type>\w+)?(#)?(?<id>(?(5)\w+))(\.)?(?<class>(?(7)\w+))(\[)?(?<attributes>(?(9).+))(?(9)\])(\+)?(?<pseudo>(?(12)\w+))$', 'names');
					assert(~isempty(selector), 'malformed selector ''%s''', act{2});
					% split the attribute qualifiers:                 '(attribute )...(         operator          )...(       ref attribute        )(    value    )( comma? )
					selector.attributes = regexp(selector.attributes, '(?<name>[\w.]+)\s*(?<operator>[\^\$\*~]?=)\s*(\$)?(?<ref>(?(3)\d+))(?(3)\.)(?<value>[^,]+)(\s*,\s*)?', 'names');
					
					if ~isempty(selector.id)
						common_find_args = [common_find_args, 'Name', ['^' selector.id '$']]; %#ok<AGROW>
					end
					
					if ~isempty(selector.type)
						assert(~strcmp(selector.type, 'Subsystem'), '''Subsystem'' is not a valid block type, use ''SubSystem''');
						common_find_args = [common_find_args 'BlockType', ['^' selector.type '$']]; %#ok<AGROW>
					end
					
					if ~isempty(selector.class)
						common_find_args = [common_find_args, 'MaskType', ['^' selector.class '$']]; %#ok<AGROW>
					end
					
					if ~isempty(selector.pseudo)
						common_find_args = [common_find_args, selector.pseudo, 'on']; %#ok<AGROW>
					end
					
					tlattrs = []; rattrs = []; rinfos = []; % collect ref-attrs and ref-infos
					for attr = selector.attributes % parameters
						if ~isempty(attr.ref) % the value is a ref-property ~> append to find_args or tlattrs later
							% value is the parameter '$1.param'
							rinfos = [rinfos; handles(str2double(attr.ref), :)]; %#ok<AGROW>
							rattrs = [rattrs attr]; %#ok<AGROW>

						else % the value is a literal ~> translate & append to find_args/tlattr now
							if numel(attr.value) >= 2 && attr.value(1) == '"' && attr.value(end) == '"' % strip quotes
								attr.value = attr.value(2:end-1);
							end
							if strncmp(attr.name, 'tl.', 3) % is a TargetLink block property
								tlattrs = [tlattrs attr]; %#ok<AGROW>
							else
								common_find_args = [common_find_args, attr.name, slQuery.wrap_find_arg(attr.operator, attr.value)]; %#ok<AGROW>
							end
						end
					end
					if ~isempty(tlattrs), common_find_args = [common_find_args, 'data', '.*']; end %#ok<AGROW>
					new_handles = double.empty(size(handles, 1) +1, 0); % height of new selection is one more
				end
				
				% new handles-array is constructed in blocks corresponding to group/info-pairs. A block is
				% generated by combining each row of the group with each search result based on info)
				[infos, ~, info_idx] = unique([hinfos' rinfos'], 'rows');
				for i = 1:size(infos, 1)
					find_args = common_find_args;
					
					info = infos(i, 1); % distinguishing hinfo of the current group
					for j = 1:numel(rattrs)
						value = get_param(infos(i, 1 + j), rattrs(j).value); % ref param of rinfo block
						find_args = [find_args, rattrs(j).name, slQuery.wrap_find_arg(rattrs(j).operator, value)]; %#ok<AGROW>
					end
					
					switch combinator.type
						case ','
							% info is the root of the previous block
							new = find_system(info, find_args{:})';
						case ' ' % sibling of the current block
							par = slQuery.get_ref(info, 'Parent');
							new = setdiff(find_system(par, 'SearchDepth', 1, find_args{:})', [par info]);
						case '/' % direct descendant (child)
							new = setdiff(find_system(info, 'SearchDepth', 1, find_args{:})', info);
						case '//' % arbitrary depth descendant
							new = setdiff(find_system(info, find_args{:})', info);
						case '\' % direct ascendant (parent)
							new = find_system(info, 'SearchDepth', 0, find_args{:})';
						case '\\' % arbitrary ascendant (ancestor)
							% compute the chain of parents
							new = [];
							while info ~= -1
								new(end+1) = info; %#ok<AGROW>
								info = slQuery.get_ref(info, 'Parent');
							end
							% filter by search-match
							new = find_system(new, 'SearchDepth', 0, find_args{:})';
							
						case {'§', '´'} % the linked library block
							% info is the reference block property already resolved
							if info == -1
								new = [];
							else
								new = find_system(info, 'SearchDepth', 0, find_args{:})';
							end
						case {'@', '`'} % model blocks linking this library block
							% this is more expensive: search from root 0 instead of bdroot
							new = find_system(0, find_args{:}, 'ReferenceBlock', ['^' getfullname(info) '$'])';
						case '-' % directly connected
							pc = get_param(info, 'PortConnectivity');
							pc(~strncmp({pc.Type}, 'RConn', 5)) = [];
							if (combinator.sp)
								pc(~strcmp({pc.Type}, ['RConn' num2str(combinator.sp)])) = [];
							end
							ps = [pc.DstPort];
							if combinator.dp
								ps(slQuery.get_param(ps, 'PortNumber') ~= combinator.dp) = [];
							end
							new = find_system(setdiff(slQuery.get_ref(ps, 'Parent'), info), 'SearchDepth', 0, find_args{:})';
						case {'->', '<-'} % directly feeding
							ps = double.empty(1, 0);
							if strcmp(combinator.type, '->')
								lines = slQuery.get_param(slQuery.get_ports(info, 'Outport', combinator.sp), 'line');
								ps = [ps slQuery.get_ports(lines(lines ~= -1), 'DstPortHandle', combinator.dp)]; %#ok<AGROW>
							end
							if strcmp(combinator.type, '<-')
								lines = slQuery.get_param(slQuery.get_ports(info, 'Inport', combinator.sp), 'line');
								ps = [ps slQuery.get_ports(lines(lines ~= -1), 'SrcPortHandle', combinator.dp)]; %#ok<AGROW>
							end
							new = find_system(slQuery.get_ref(ps, 'Parent'), 'SearchDepth', 0, find_args{:})';
						case {  '~>', '~', '<~' ... indirectly wired, including virtual blocks (Subsystem-Levels, Goto/From, BusCreator/BusSelector, Mux/Demux)
								'=>', '=', '<=' ... logically wired, excluding virtual blocks
								'>>', '<>', '<<' ... signal slicing (any data dependencies)
								}
							ps = double.empty(1, 0);
							virt = ismember(combinator.type, {'~>', '~', '<~', '<<', '<>', '>>'});
							slic = ismember(combinator.type, {'<<', '<>', '>>'});
							% make empty frontier for recursion breakpoints
							front = containers.Map('KeyType', 'char', 'ValueType', 'logical');
							if ismember(combinator.type, {'~>', '~', '=>', '=', '>>', '<>'})
								for port = [-info(ismember(get_param(info, 'BlockType'), {'Outport', 'Goto'})) slQuery.get_ports(info, 'Outport', combinator.sp)]
									ps = [ps slQuery.follow_signal(port, {}, front, virt, slic)]; %#ok<AGROW>
								end
							end
							if ismember(combinator.type, {'<~', '~', '<=', '=', '<<', '<>'})
								for port = [-info(ismember(get_param(info, 'BlockType'), {'Inport', 'From'})) slQuery.get_ports(info, 'Inport', combinator.sp)]
									ps = [ps slQuery.follow_signal(port, {}, front, virt, slic)]; %#ok<AGROW>
								end
							end
							% filter by port handle number
							if ~isempty(combinator.dp)
								if isnumeric(combinator.dp)
									ps = ps(slQuery.get_param(ps, 'PortNumber') == combinator.dp);
								elseif ischar(combinator.dp) && isequal(combinator.dp, '!')
									ps = ps(slQuery.get_param(ps, 'PortNumber') == combinator.dp);
								end
							end
							% negative return values of follow are the handles of port blocks
							new = [-ps(ps < 0), slQuery.get_ref(ps(ps>0), 'Parent')];
							new = find_system(new, 'SearchDepth', 0, find_args{:})';
					end
					new = reshape(unique(new), 1, []); % reshape for ML 2010b unique
					group = handles(:, info_idx == i);
					
					% filter TargetLink properties via the tlattrs 
					if ~isempty(tlattrs)
						for attr = tlattrs
							% new(arrayfun(@(h) ~isfield(get_param(h, 'ObjectParameters'), 'data'), new)) = [];
							new(arrayfun(@(h) ~ismember(attr.name(4:end), tl_get(h)), new)) = [];
							regex = slQuery.wrap_find_arg(attr.operator, attr.value);
							new(arrayfun(@(h) isempty(regexp(char(tl_get(h, attr.name(4:end))), regex, 'once')), new)) = [];
						end
					end
					
					if isnumeric(selector) % selector was reference ~> pick only columns, where the ref matches to one of the results
						group = group(:, ismember(group(selector, :), new));
					else %if isstruct(selector) ~> append block corresponding to this group
						
						% TODO: perf: move this intersect-step upwards, to avoid unneccesary `find_system`-calls when
						% there aren't any restrictions OR if there is a candidate set, simply test each candidate
						% against the set of restrictions using find_system.
						if ~isempty(selector.argidx)
							new = intersect(new, varargin{str2double(selector.argidx)});
						end
						% outer join of group and new elements
						group = [repmat(group, 1, size(new, 2)); repelem(new, 1, size(group, 2))];
					end
					new_handles = [ new_handles, group ]; %#ok<AGROW>
				end
				
				% next selector step: handles from new_handles, hot from last selector results
				handles = new_handles;
				if isnumeric(selector)
					hot = handles(selector, :);
				else
					hot = handles(size(handles, 1), :);
				end
			end
		end
		
		function res = listfun(fun, varargin)
			% perform an arrayfun call, where each call may return multiple handles and concat all the
			% results into a single array
			res = arrayfun(fun, varargin{:}, 'UniformOutput', false);
			res = [res{:}];
			if isempty(res), res = double.empty(1,0); end
		end
		
		function varargout = arrayfun(fun, varargin)
			% perform an arrayfun call after coercing the arguments to the same size
			cs = cellfun(@ischar, varargin);
			sz = cellfun(@(x) [size(x,1), size(x,2)], varargin(~cs), 'UniformOutput', false);
			sz = vertcat(sz{:}, [1 1]); % neutral element if all args are strings (scalar case)
			assert(numel(setdiff(sz(:, 1), 1)) <= 1 && numel(setdiff(sz(:, 2), 1)) <= 1, ...
				'MATLAB:dimagree', 'Non singleton dimensions of assignment value must match selection.');
			
			if any(sz(:) == 0) % some parameter is empty ~> empty result
				[r, ~] = find(sz == 0);
				[varargout{1:nargout}] = deal(double.empty(sz(r(1),:)));
			else
				sz = max(sz, [], 1);
				for i = 1:numel(varargin)
					v = varargin{i};
					if cs(i), v = {v}; end % convert to cellstr
					if isrow(v), v = repmat(v, sz(1), 1); end
					if iscolumn(v), v = repmat(v, 1, sz(2)); end
					if ~iscell(v), v = num2cell(v); end
					varargin{i} = v;
				end
				[varargout{1:nargout}] = cellfun(fun, varargin{:});
			end
		end
		
		function res = get_param(hs, param)
			% a vectorial get_param without scalar/empty-quirks
			res = arrayfun(@(h) get_param(h, param), double(hs), 'UniformOutput', false);
			if iscell(res) && ~iscellstr(res), res = vertcat(res{:}); end
			if isempty(res), res = double.empty(1,0); end
		end

		function type = get_paramtype(sel, param)
			% TODO: maybe check, that all blocks actually have this parameter
			if isempty(sel)
				type = '';
			elseif ismember(lower(param), {'object', 'linkdata'}) % not listed in ObjectParameters
				type = 'object';
			else
				ops = get_param(sel(1), 'ObjectParameters');
				fs = fieldnames(ops);
				fi = strcmpi(param, fs); assert(any(fi), 'no parameter ''%s''', param);
				type = ops.(fs{fi}).Type;
			end
		end
		
		function r = get_ref(hs, reftype)
			% resolve properties that represent a block to their handle
			p = get_param(hs, reftype);
			if ischar(p), p = {p}; end
			
			% unset references will resolve to the handle -1
			r = zeros(size(hs));
			i = cellfun(@isempty, p);
			r(i) = -1; 
			r(~i) = cell2mat(get_param(p(~i), 'Handle'));
		end
		
		function value = wrap_find_arg(operator, value)
			switch operator
				case '=',  value = ['^' regexptranslate('escape', value) '$']; % literal match "text"
				case '^=', value = ['^' regexptranslate('escape', value)]; % literal match at beginning "text*"
				case '$=', value = [regexptranslate('escape', value) '$']; % literal match at the end "*text"
				case '*=', value = regexptranslate('escape', value); % literal match anywhere "*text*"
				case '~=' % regex match,, value shall be a regexp already
			end
		end
		
		% signal slicing frontier is a set of blocks, already reached during current
		% "follow"-call. NOTE: _re_cursive calls are ok, but no _con_current calls of
		% slQuery.follow may occur (follow itself must not for some reason want to start a new
		% search)
		
		% NOTE: negative values in bp and eps represent a the "non-wired" part of a virtual routing
		% aspect. E.g. an Outport- or Goto-block itself hasn't got an outbound port, whose handle we can
		% pass here. Hence, the negative of that block's handle is used as a stand-in.
		function eps = follow_signal(bp, addr, front, virt, slic)
			% follow the data flow of a port through lines and virtual blocks OutPort/Subsystem,
			% Subsystem/InPort, Goto/From, Mux/Demux, BusCreator/Selector, ...
			assert(isnumeric(bp) && isscalar(bp)); % bp - "begin"-port, whose line we want to follow
			assert(iscell(addr)); % addr - the address of a compound signal (bus, array)
			assert(islogical(virt)); % whether to include virtual routing blocks in the result
			assert(islogical(slic)); % whether to also follow data dependency (instead of only data flow)
			
			eps = []; % eps is the list of "end"-ports gathered from following bp to where it leads
			if bp < 0 % non-wired connection as startpiont
				if ~strcmp(get_param(-bp, 'Type'), 'block'), return, end % bail out at block diagram level
				switch get_param(-bp, 'BlockType')
					case {'Goto', 'Outport'}
						[edir, pdir] = deal('Inport', 'Outport');
					case {'From', 'Inport'}
						[edir, pdir] = deal('Outport', 'Inport');
					otherwise
						return % these blocks aren't non-wired
				end
				beps = slQuery.get_ports(-bp, edir, 1);
			
			else % this is a port handle ~> follow the lines
				ls = get_param(bp, 'Line');
				ls(ls == -1) = []; % don't follow unconnected lines
				
				% the 'PortType' determines the direction of following (upstream is true, downstream is false, )
				if strcmp(get_param(bp, 'PortType'), 'inport')
					[edir, pdir, beps] = deal('Outport', 'Inport', slQuery.get_ports(ls, 'SrcPortHandle'));
				else
					[edir, pdir, beps] = deal('Inport', 'Outport', slQuery.get_ports(ls, 'DstPortHandle'));
				end
			end
			
			% construct 'signal fiber' identifier: addr tokens (names/mux-indices) as single string
			fibid = addr;
			an = cellfun(@isnumeric, fibid);
			fibid(an) = cellfun(@(x) {['[' char(typecast(x, 'uint8')) ']']}, fibid(an));
			fibid(~an) = strcat('.', fibid(~an));
			fibid = [fibid{:}];
			
			for ep = beps'
				b = slQuery.get_ref(ep, 'Parent');
				
				sigid = [char(typecast(ep, 'uint8')) ':' fibid];
				if front.isKey(sigid), continue, end % fully came around ~> prune traversal here
				front(sigid) = true;
				
				% handle all the virtual (signal routing) blocks
				switch get_param(b, 'BlockType')
					case {'Inport', 'Outport'} % a port ~> follow it outside
						s = slQuery.get_ref(b, 'Parent');
						if strcmp(get_param(s, 'Type'), 'block')

							if ~isempty(get_param(s, 'VariantControl')) % system is variant ~> skip to parent
								% TODO: include the port block on VariantSystem level into neps
								s = slQuery.get_ref(s, 'Parent');
							end
							sp = slQuery.get_ports(s, pdir, str2double(get_param(b, 'Port')));
							neps = slQuery.follow_signal(sp, addr, front, virt, slic);
							
						else % port is on the block_diagram level ~> cannot go beyond, done
							neps = ep;
						end
						if virt, neps = [neps -s]; end %#ok<AGROW>
						
					case 'SubSystem' % ~> follow it inside
						% skip events/actions/triggers (for now)
						if ~ismember(get_param(ep, 'PortType'), {'inport', 'outport'}), continue; end
						pn = num2str(get_param(ep, 'PortNumber'));

						if strcmp(get_param(b, 'Variant'), 'on') % it's a variant sub-system and all variants must be followed
							ss = setdiff(find_system(b, 'LookUnderMasks', 'all', 'FollowLinks', 'on', 'Variants', 'all', 'SearchDepth', 1, 'BlockType', 'SubSystem')', b);
						else
							ss = b;
						end

						neps = [];
						for s = ss
							pbs = find_system(s, 'LookUnderMasks', 'all', 'FollowLinks', 'on', 'SearchDepth', 1, 'BlockType', edir, 'Port', pn);
							for bp = slQuery.get_ports(pbs, pdir, 1)
								neps = [neps, slQuery.follow_signal(bp, addr, front, virt, slic)]; %#ok<AGROW>
							end
						end

						if virt, neps = [neps -pbs]; end %#ok<AGROW>
						
					case 'Goto' % ~> find corresponding From-blocks
						tag_args = {'LookUnderMasks', 'all', 'FollowLinks', 'on', 'GotoTag', get_param(b, 'GotoTag')};
						switch get_param(b, 'TagVisibility')
							case 'local'
								fbs = find_system(slQuery.get_ref(b, 'Parent'), 'SearchDepth', 1, tag_args{:}, 'BlockType', 'From');
								
							case 'scoped'
								% find the corresponding scope-block
								s = slQuery.get_ref(b, 'Parent');
								while s ~= -1
									if ~isempty(find_system(s, 'SearchDepth', 1, tag_args{:}, 'BlockType', 'GotoTagVisibility'))
										break;
									end
									s = slQuery.get_ref(s, 'Parent');
								end
								
								if s == -1, continue, end % no scope found ~> no more s-levels, break
								
								% limiting scope block subsystems,
								sbss = find_system(s, tag_args{:}, 'BlockType', 'GotoTagVisibility');
								sbss = slQuery.get_ref(sbss(2:end), 'Parent'); % don't count the actually effective scope block
								
								% now all from blocks but not the ones inside one of sbss, TODO: perf.
								fbs = setdiff( ...
									find_system(s, tag_args{:}, 'BlockType', 'From'), ...
									find_system(sbss, tag_args{:}, 'BlockType', 'From') ...
									, 'rows');
								
							case 'global'
								fbs = find_system(bdroot(b), tag_args{:}, 'BlockType', 'From');
								
						end
						neps = slQuery.listfun(@(fp) slQuery.follow_signal(fp, addr, front, virt, slic), slQuery.get_ports(fbs, 'Outport', 1));
						if virt, neps = [neps -fbs']; end %#ok<AGROW>
						
					case 'From' % ~> find corresponding Goto-blocks
						tag_args = {'LookUnderMasks', 'all', 'FollowLinks', 'on', 'GotoTag', get_param(b, 'GotoTag')};
						
						% determine earliest innermost Tag-Visibility
						s = slQuery.get_ref(b, 'Parent');
						% find the corresponding scope-block
						while s ~= -1
							if ~isempty(find_system(s, 'SearchDepth', 1, tag_args{:}, 'BlockType', 'GotoTagVisibility'))
								break;
							end
							s = slQuery.get_ref(s, 'Parent');
						end
						
						if s ~= -1 % there is a matching scope
							% find limiting scope block subsystems
							sbss = find_system(s, tag_args{:}, 'BlockType', 'GotoTagVisibility');
							sbss = slQuery.get_ref(sbss(2:end), 'Parent'); % don't count the actually effective scope block
							
							gbs = setdiff( ...
								find_system(s, tag_args{:}, 'BlockType', 'Goto'), ...
								find_system(sbss, tag_args{:}, 'BlockType', 'Goto') ...
								, 'rows');
							
						else % no scope found ~> local or global Gotos
							gbs = [ ...
								find_system(slQuery.get_ref(b, 'Parent'), 'SearchDepth', 1, tag_args{:}, 'TagVisibility', 'local', 'BlockType', 'Goto'), ...
								find_system(bdroot(b), tag_args{:}, 'TagVisibility', 'global', 'BlockType', 'Goto') ...
								];
							
						end
						
						if isempty(gbs), continue, end
						if ~isscalar(gbs), warning('ambiguous Goto blocks (%s)!', strjoin(gbs, ', ')); end
						
						neps = slQuery.listfun(@(bp) slQuery.follow_signal(bp, addr, front, virt, slic), slQuery.get_ports(gbs, 'Inport', 1));
						if virt, neps = [neps -gbs]; end %#ok<AGROW>
						
					case {'Mux', 'Demux'}
						if strcmp(get_param(b, 'BlockType'), 'Mux') == strcmp(edir, 'Inport') % array packing direction ~> add an addr token
							neps = slQuery.follow_signal(slQuery.get_ports(b, pdir), [addr {get_param(ep, 'PortNumber')}], front, virt, slic);
							
							% ~> array unpacking direction
							
						elseif isempty(addr) % but there is no addr token for digging down
							% when signal-slicing, we must follow all components of an ending mux-signal, even though
							% the signal itself has ended (blocks behind this Mux/Demux are indeed in the signal slice of
							% the original block)
							if slic
								neps = slQuery.listfun(@(bp) slQuery.follow_signal(bp, addr, front, virt, slic), ...
									slQuery.get_ports(b, pdir));
							else
								neps = [];
							end
							
						elseif isscalar(slQuery.get_ports(b, pdir)) % only one inport and outport ~> doesn't mux/demux anything at all
							neps = slQuery.follow_signal(slQuery.get_ports(b, pdir, 1), addr, front, virt, slic);
							%TODO: handle one-dimensional arrays properly
							
						else % a valid addr ~> try to match the array token
							assert(isnumeric(addr{end}));
							neps = slQuery.listfun(@(bp) slQuery.follow_signal(bp, addr(1:end-1), front, virt, slic), slQuery.get_ports(b, pdir, addr{end}));
						end
					%case 'Assignment'
					%case 'Concatenate'
						
					case 'BusCreator'
						if strcmp(edir, 'Inport') % ~> add an address token and follow the outport
							signals = get_param(b, 'SignalHierarchy');
							neps = slQuery.follow_signal(slQuery.get_ports(b, 'Outport', 1), [addr signals(get(ep, 'PortNumber')).name], front, virt, slic);
							
						elseif isempty(addr) % there is no addr element for digging down the bus
							if slic % signal slicing ~> follow the opposite ports of this endpoint (even though, the bus is now many signals)
								bps = slQuery.get_ports(b, pdir); % all ports of the other side
								neps = slQuery.listfun(@(bp) slQuery.follow_signal(bp, addr, front, virt, slic), bps);
								
							else % no signal slicing ~> we're done (this block is it)
								neps = ep;
								
							end
							
						else % that addr token must be in the names
							signals = get_param(b, 'SignalHierarchy');
							neps = slQuery.listfun(@(bp) slQuery.follow_signal(bp, addr(1:end-1), front, virt, slic), ...
								slQuery.get_ports(b, 'Inport', strcmp(addr{end}, {signals.name})));
						end
						
					case 'BusSelector'
						if strcmp(get_param(b, 'OutputAsBus'), 'on') % selector makes a new bus using maybe fewer signals but the same adresses
							neps = slQuery.follow_signal(slQuery.get_ports(b, pdir, 1), addr, front, virt, slic);
							
						else % must consider signal adresses
							% OutputSignals is a comma-separated list of period-separated signal pathnames
							pathes = strsplit(get_param(b, 'OutputSignals'), ',');
							if strcmp(edir, 'Inport') % some of the output signals may match the addr token and we will follow them
								% there is an address try and find the matching set of tokens
								neps = double.empty(1, 0);
								for ti = numel(addr):-1:1
									if ~ischar(addr{ti}), break, end % out of name tokens, cannot go further down
									for spi = find(strcmp(strjoin(addr(end:-1:ti), '.'), pathes)) % indices of ports referencing that exact signal
										neps = [neps slQuery.follow_signal(slQuery.get_ports(b, 'Outport', spi), addr(1:ti-1), front, virt, slic)]; %#ok<AGROW>
									end
								end
							else % ~> add corresponding name tokens to addr
								tokens = fliplr(strsplit(pathes{get_param(ep, 'PortNumber')}, '.'));
								neps = slQuery.follow(slQuery.get_ports(b, 'Inport', 1), [addr tokens], front, virt, slic);
							end
						end
						
					case 'BusAssignment'
						pathes = strsplit(get_param(b, 'AssignedSignals'), ',');
						if strcmp(edir, 'Inport') && get_param(ep, 'PortNumber') > 1 % ~> it's one of the substitution element ports
							neps = slQuery.follow_signal(slQuery.get_ports(b, pdir, 1), [addr pathes{get(ep, 'PortNumber') - 1}], front, virt, slic);
						else % it's the bus port (whole bus)
							if isempty(addr) % there is no addr token for digging down and because original the bus as-such doesn't continue, we're done
								% TODO: does this make any sense?
								neps = ep;
							else
								% is the selected element one of the substituted ones?
								[~, selidx] = ismember(addr{end}, pathes);
								if selidx == 0 % signal wasn't selected ~> follow opposite bus port (pdir)
									neps = slQuery.follow_signal(slQuery.get_ports(b, pdir, 1), addr, front, virt, slic);
									
									% signal name was substituted
								elseif strcmp(edir, 'Inport') % substitution has cancelled this name downstream
									continue;
								else % follow the respective input port upstream
									neps = slQuery.follow_signal(slQuery.get_ports(b, pdir, selidx + 1), addr(1:end-1), front, virt, slic);
								end
							end
						end
						
					case {'DataTypeConversion', 'InitialCondition', 'SignalConversion', 'SignalSpecification'} % nonvirtual "through-blocks"
						neps = slQuery.follow_signal(slQuery.get_ports(b, pdir, 1), addr, front, virt, slic);
						
					otherwise
						if slic % signal slicing ~> follow the opposite ports of this endpoint
							
							% addr should be emptied, because signal hierarchies don't relate to each other in the
							% "otherwise" case. TODO: this is dependent on the block type and config (e.g. mux-signals of
							% Gain in element-wise mode are decoupled)
							naddr = {};
							
							neps = [ep slQuery.listfun(@(bp) slQuery.follow_signal(bp, naddr, front, virt, slic), ...
								slQuery.get_ports(b, pdir))]; % all ports of the other side
							
							front(sigid) = true;
						else
							neps = ep;
						end
				end
				
				% when including virtual routing, the immediate port pdir direction (ep) must also be counted
				eps = unique([eps ep(virt) neps]);
			end
		end
		
		function ps = get_ports(hs, type, index)
			% get the port handles attaching to blocks or lines
			% hs - the blocks or lines
			% type ('Inport', 'Outport', 'DstPortHandle', 'SrcPortHandle')
			% index - index or label of the ports in the attaching block
			
			% get all port handles
			if ~isempty(hs) && strcmp(get_param(hs(1), 'Type'), 'block_diagram') % diagrams don't have external ports
				ps = [];
			elseif strcmp(type, 'SrcPortHandle') || strcmp(type, 'DstPortHandle')
				ps = slQuery.get_param(hs, type);
			elseif isempty(hs)
				ps = double.empty(0, 1);
			else
				ps = get_param(hs, 'PortHandles');
				if iscell(ps), ps = [ps{:}]; end
				if ~isstruct(ps)
				end
				ps = [ps.(type)]; % catted horizontally
			end
			
			% filter by correct port index
			if nargin < 3 || strcmp(index, '') % no index ~> keep all handles
			else
				assert(~islogical(index) || isscalar(hs));
				assert(ischar(index) || isnumeric(index) || islogical(index));
				
				if islogical(index)
					ps = ps(index);
				elseif isnumeric(index)
					ps = ps(arrayfun(@(p) get_param(p, 'PortNumber') == index, ps));
				elseif isequal(index, '!') % trigger
					ps = ps(strcmp(slQuery.get_param(ps, 'PortType'), 'trigger'));
				elseif isequal(index, '?') % enable
					ps = ps(strcmp(slQuery.get_param(ps, 'PortType'), 'enable'));
				elseif isequal(index, '%') % enable
					ps = ps(strcmp(slQuery.get_param(ps, 'PortType'), 'ifaction'));
				elseif isequal(index, '°') % enable
					ps = ps(strcmp(slQuery.get_param(ps, 'PortType'), 'Reset'));
				else % char ~> filter by port name, when the block is a subsystem
					if strcmp(type, 'Inport') || strcmp(type, 'DstPortHandle')
						bt = 'Inport';
					else
						bt = 'Outport';
					end
					
					pbs = arrayfun( ...
						@(p) find_system(get_param(p, 'Parent'), 'LookUnderMasks', 'all', 'FollowLinks', 'on', 'SearchDepth', 1, 'BlockType', bt, 'Port', num2str(get_param(p, 'PortNumber'))) ...
						, ps, 'UniformOutput', false ...
						);
					
					ps = ps(~cellfun(@isempty, pbs)); pbs = [pbs{:}];
					ps = ps(strcmp(get_param(pbs, 'Name'), index));
				end
			end
			
			if isempty(ps); ps = double.empty(1, 0); end
		end
	end
	methods(Static) % for convenience
		function result = gcb, result = slQuery(gcbh); end
		function result = gcs, result = slQuery(get_param(gcs, 'Handle')); end
		function result = sel
			result = find_system(get_param(gcs, 'Handle'), 'SearchDepth', 1, 'Selected', 'on');
			result = slQuery(result(2:end)');
		end
	end
end

% the server crashes if the user's password is a resolvable url
