%{
                               _  ___
                           ___| |/ _ \ _   _  ___ _ __ _   _
                          / __| | | | | | | |/ _ \ '__| | | |
                          \__ \ | |_| | |_| |  __/ |  | |_| |
                          |___/_|\__\_\\__,_|\___|_|   \__, |
                           easy-as-pie API to Simulink |___/

v0.9.4, 2017 robert@raschhour.com

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
			elseif ischar(query) % normal query
				% additional arguments represent sets of blocks to pick from via ^123 - index
				idx = cellfun(@ischar, varargin);
				varargin(idx) = cellfun(@(s) {get_param(s, 'Handle')}, varargin(idx));
				idx = cellfun(@iscellstr, varargin);
				varargin(idx) = cellfun(@(cs) {cell2mat(get_param(cs, 'Handle'))}, varargin(idx));
				assert(~isempty(bdroot), 'no simulink diagram is active');
				handles = slQuery.select(query, varargin{:});
			else
				error('illegal arguments');
			end
			
			this = this@double(handles);
		end
	end
	methods (Access='public', Hidden=true)
		function disp(this)
			if isempty(this)
				fprintf('   Empty slQuery: 0-by-%d\n', size(this, 2));
			else
				disp('slQuery with handles')
				
				x = dbstack; % hyperlinks don't look nice in datatip displays
				if numel(x) >= 2 && strncmp(x(2).name, 'datatipinfo', 11)
					disp(double(this));
				else
					% must build one big text, so that it can print immediately
					mag = floor(log10(max(max(double(this)))));
					repr = arrayfun(@(h, l) sprintf('    <a href="matlab: hilite_system(%.15f);">%1.4f</a>', h, l), ...
						double(this)', double(this)' / 10.^mag, 'UniformOutput', false);
					repr(end+1, :) = {char(10)}; disp([repr{:}])
				end
			end
		end
		
		function slice = subsref(this, indices)
			slice = this; % initial selection for get_param
			for sel = indices;
				switch sel.type
					case '()' % selection
						if isa(slice, 'slQuery')
							if isscalar(sel.subs) && isnumeric(sel.subs{1}) && ~isvector(slice)
								sel.subs = [':', sel.subs];
							elseif isscalar(sel.subs) && islogical(sel.subs{1})
								sel.subs = [sel.subs, ':'];
							end
							slice = slQuery(builtin('subsref', double(slice), sel));
						else
							slice = builtin('subsref', slice, sel);
						end
						
					case '.' % parameter access
						switch sel.subs
							case {'show', 'showall'} % show the situations line by line
								cs = gcs;
								for row = double(slice')
									hilite_system(row);
									if strcmp(sel.subs, 'showall')
										disp(row'); pause(0.5);
									else
										pause
									end
									hilite_system(row, 'none');
								end
								open_system(cs);
							case 'hyperlink' % an array of links to the blocks (with their name)
								slice = arrayfun( ...
									@(h) sprintf('<a href="matlab: hilite_system(%.15f);">%s</a>', h, ...
									strrep(get_param(h, 'Name'), char(10), ' ')), ...
									double(slice), 'UniformOutput', false);
								
							case 'handle' % most of the simulink functions can't handle objects derived from double as double handles.
								slice = double(slice);
								
							case 'wrap' % TODO: is this cool?
								slice = slQuery(slice);

							case 'fullname' % return the model path of all blocks
								slice = getfullname(double(slice));
								
							case 'tl' % return a TargetLink property
								% TODO: the property hierarchy must to be resolved before (outside of this loop)
								slice = arrayfun(@(h) tl_get(h, 'blockdatastruct'), double(slice), 'UniformOutput', false);
								
							otherwise % select a block parameter, field of struct or property of object
								if isnumeric(slice) % parameter of a block handle
									slice = arrayfun(@(h) get_param(h, sel.subs), double(slice), 'UniformOutput', false);
									
								elseif iscellstr(slice) % possibly block path (ReferenceBlock, Ancestor-Block, Parent, ...)
									slice = cellfun(@(h) get_param(h, sel.subs), slice, 'UniformOutput', false);
									
								elseif isstruct(slice) || isobject(slice) || all(ishandle(slice)) % field of a struct or property of an object
									slice = arrayfun(@(h) h.(sel.subs), slice, 'UniformOutput', false);
									
								elseif iscell(slice) && all(cellfun(@isobject, slice) | ishandle(slice)) % (hopefully) shared member of collection of different objects
									slice = cellfun(@(h) h.(sel.subs), slice, 'UniformOutput', false);
								end
						end
						
						% TODO: when the result is a cell but happens to be a uniform (e.g.
						% LineHandles.Outport of a bunch of inport blocks), the result cab be a
						% uniform double-array instead of a cell nesting all the single entries.
						% This is counter-intuitive for LineHandles (of arbitrary blocks) but
						% not for e.g. Position.
						% TODO: When accessing a char-parameter from a single 1x1 slQuery-handle
						% user expects a char, not a 1x1 cellstr mostly. But there could be
						% cases, where a list of blocks happens to be 1-long and the property
						% must be a cellstr. Maybe all results should be in cells unless each
						% one is scalar by itself (like numbers). Maybe add syntax so we can be
						% explicit for this. 
					case '{}' % actions
						
					otherwise % they did something stupid
						error(['what is ''' sel.type '''?']);
				end
				
				% coerce the result to a common convenient data type
				if iscell(slice)
					if isscalar(slice)
						slice = slice{1};
					else
						type = unique(cellfun(@class, slice, 'UniformOutput', false));
						
						if ~isscalar(type), continue, end
						
						if isnumeric(slice{1})
							slice = cell2mat(slice);
						elseif isstruct(slice{1})
							fields = cellfun(@fieldnames, slice, 'UniformOutput', false);
							if ~isequal(fields{:}), return, end
							slice = cell2mat(slice);
						end
					end
				end
			end
		end
		
		function this = subsasgn(this, indices, value)
			% initial selection  for get_param
			slice = double(this);
			for sel = indices
				switch sel.type
					case '()' % selection
						if isscalar(sel.subs) && isnumeric(sel.subs{1})
							sel.subs = [':', sel.subs];
						elseif isscalar(sel.subs) && islogical(sel.subs{1})
							sel.subs = [sel.subs, ':'];
						end
						slice = builtin('subsref', slice, sel);
						
					case '.' % parameter access
						if isscalar(value) || ischar(value)% scalar assignment ~> propagate everywhere
							
							arrayfun(@(h) set_param(h, sel.subs, value), double(slice));
							
						elseif isvector(value) % vector assignment ~> propagate to columns or rows
							assert(any(numel(value) == size(slice)), 'MATLAB:dimagree', 'Number of columns or rows in value and selection must match.');
							
							% if number of rows matches, transpose the selection (and values)
							if numel(value) == size(slice, 1)
								slice = slice';
								value = value';
								
								% Note: An assignment with matching row-count doesn't generally make much sense, because the
								% rows-dimension is for the arbitrary number of results found by a query, i.e. it is
								% unpredictable. We do however still support this mode.
							end
							
							for i = 1:size(value, 2)
								arrayfun(@(h) set_param(h, sel.subs, value{i}), double(slice(:, i)));
							end
						
						else % matrix assignment ~> row/col-wise or exact assignments
							sm = size(value) == size(slice); % matching dimensions
							
							if all(sm)
								arrayfun(@(h, v) set_param(h, sel.subs, value{1}), slice, value);
								
							elseif any(sm)
								if sm(2), value = value'; end
									
								for i = 1:size(value, 1)
									set_param(slice(i, :), sel.subs, value(i, :));
								end
							else
								error('MATLAB:dimagree', 'Value shape must be scalar, row vector or match selection.');
							end
						end
					case '{}' % actions
						
					otherwise % they did something stupid
						error(['what is ''' sel.type '''?']);
				end
			end
		end
	end
	
	methods(Access=private, Static)
		
		function handles = select(query, varargin) % core "select" algorithm of slQuery
			% split query along the combinators                                                                                          ( outside [] )
			[selectors, combinators] = regexp(strtrim(query), '\s*( |\\\\|\\|//|/|(:\s*\w+\s*)?(->|-|<-|~>|~|<~|=>|<=|>>|<>|<<|,)(\s*\w+\s*:)?)\s*(?![^\[]*\])', 'split', 'match');
			
			% start with the search root and combinator ',' for arbitrary position in this root
			root = get_param(bdroot, 'Handle'); % always search only in current model
			handles = double.empty(1, 0);
			hot_col = root;
			for row = [',' combinators; selectors]
				% parse the combinator:     '    (colon with portspec)...(                      combinator type (again)                       )...(portspec with colon )
				combinator = regexp(row{1}, '^\s*(:)?(?<sp>(?(1)\w+))?\s*(?<type>( |\\\\|\\|//|/|->|-|<-|~>|~|<~|=>|<=|>>|<>|<<|,(?![^\[]*\])))\s*(?<dp>\w+)?\s*(?(4):)?\s*$', 'names');
				
				% cast numeric port qualifiers
				if ~isnan(str2double(combinator.sp)), combinator.sp = str2double(combinator.sp); end
				if ~isnan(str2double(combinator.dp)), combinator.dp = str2double(combinator.dp); end
				
				% build the find_system filter from blockspec (all searches are reduced to this)
				find_args = {'FollowLinks', 'on', 'LookUnderMasks', 'all', 'Variants', 'All', 'IncludeCommented', 'on', 'Regexp', 'on'};
				
				% combinators always represent a search relating to some set of properties from the row of
				% blocks selected previously. In order to avoid multiple seaches based on the same infos (that
				% will result in the same outcome), we can group all rows based on this distinguishing info and
				% perform the search with this and then outer-join the result to the original set of rows
				switch combinator.type
					case ',' % group all into one, because not really a combinator
						cinfos = repmat(root, size(hot_col));
					case {'\', '\\'} % group by parent of the last blocks
						% NOTE/TODO: the sibling-combinator ' ' can't be here included here because each block cannot be included amongst its own siblings
						cinfos = slQuery.get_parent(hot_col);
					otherwise % group only by block-handle itself (last column)
						cinfos = hot_col;
				end
				
				if regexp(row{2}, '^\$\d+$', 'match', 'once') % selector is a reference ~> simple column-number
					selector = str2double(row{2}(2:end));
					new_handles = double.empty(0, size(handles, 2));
					
				else % selector is real ~> a real structure
					% parse as selector:       ^(*)(    parens around arg index     )?(block type)?(  hash with name  )?( period with masktype )?(    brackets and qualifier list  )?(  plus and pseudo-class )?$
					selector = regexp(row{2}, '^\*?(\()?(?<argidx>(?(1)\d+))?(?(1)\))?(?<type>\w+)?(#)?(?<id>(?(5)\w+))?(\.)?(?<class>(?(7)\w+))?(\[)?(?<attributes>(?(9).+))(?(9)\])(\+)?(?<pseudo>(?(12)\w+))?$', 'names');
					assert(~isempty(selector), 'malformed selector ''%s''', row{2});
					% split the attribute qualifiers:                 '(attribute )...(          operator           )...(    value    )( comma? )
					selector.attributes = regexp(selector.attributes, '(?<name>\w+)\s*(?<operator>(=|\^=|\$=|\*=|~=))\s*(?<value>[^,]+)(\s*,\s*)?', 'names');
					
					if ~isempty(selector.id)
						find_args = [find_args, 'Name', ['^' selector.id '$']]; %#ok<AGROW>
					end
					
					if ~isempty(selector.type)
						find_args = [find_args 'BlockType', ['^' selector.type '$']]; %#ok<AGROW>
					end
					
					if ~isempty(selector.class)
						find_args = [find_args, 'MaskType', ['^' selector.class '$']]; %#ok<AGROW>
					end
					
					if ~isempty(selector.pseudo)
						find_args = [find_args, selector.pseudo, 'on']; %#ok<AGROW>
					end
					
					for attr = selector.attributes % parameters
						if attr.value(1) == '"' && attr.value(end) == '"'
							attr.value = attr.value(2:end-1);
						end
						
						switch attr.operator
							case '=' % literal match "text"
								attr.value = ['^' regexptranslate('escape', attr.value) '$'];
							case '^=' % literal match at beginning "text*"
								attr.value = ['^' regexptranslate('escape', attr.value)];
							case '$=' % literal match at the end "*text"
								attr.value = [regexptranslate('escape', attr.value) '$'];
							case '*=' % literal match anywhere "*text*"
								attr.value = regexptranslate('escape', attr.value);
							case '~=' % regex match
								% attr.value shall be a regexp already
						end
						find_args = [find_args, attr.name, attr.value]; %#ok<AGROW>
					end
					
					new_handles = double.empty(0, size(handles, 2) +1);
				end
				
				% assert(isempty(refattrs) || strcmp(combinator.type, ','), ...
				%    'cannot use back reference in property selector after combinator other than '',''!');
				
				% the new handles-array is constructed in blocks corresponding to groups. A block is generated
				% by combining each row of the group with each result from the combinator search (based on info)
				[infos, ~, info_idx] = unique(cinfos);
				for i = 1:numel(infos)
					info = infos(i); % distinguishing info of the current group
					group = handles(info_idx == i, :);
					
					switch combinator.type
						case ','
							new_col = find_system(root, find_args{:});
							
						case ' ' % sibling of the current block
							new_col = setdiff(find_system(slQuery.get_parent(info), 'SearchDepth', 1, find_args{:}), info, 'rows');
							
						case '/' % direct descendant (child)
							new_col = setdiff(find_system(info, 'SearchDepth', 1, find_args{:}), info, 'rows');
							
						case '//' % arbitrary depth descendant
							new_col = setdiff(find_system(info, find_args{:}), info, 'rows');
							
						case '\' % direct ascendant (parent)
							new_col = find_system(info, 'SearchDepth', 0, find_args{:});
							
						case '\\' % arbitrary ascendant (ancestor)
							% compute the chain of parents
							new_col = [];
							while info % ~= root?
								new_col(end+1) = info; %#ok<AGROW>
								info = slQuery.get_parent(info);
							end
							% reduce by search-match
							new_col = find_system(new_col, 'SearchDepth', 0, find_args{:});
							
						case {'->', '-', '<-'} % directly wired
							ps = [];
							if ismember(combinator.type, {'->', '-'})
								lines = slQuery.get_param(slQuery.get_ports(info, 'Outport', combinator.sp), 'line');
								ps = slQuery.get_ports(lines(lines ~= -1), 'DstPortHandle', combinator.dp);
							end
							if ismember(combinator.type, {'<-', '-'})
								lines = slQuery.get_param(slQuery.get_ports(info, 'Inport', combinator.sp), 'line');
								ps = slQuery.get_ports(lines(lines ~= -1), 'SrcPortHandle', combinator.dp);
							end
							new_col = find_system(slQuery.get_parent(ps), 'SearchDepth', 0, find_args{:});
							
						case {  '~>', '~', '<~' ... indirectly wired, including virtual blocks (Subsystem-Levels, Goto/From, BusCreator/BusSelector, Mux/Demux)
								'=>', '=', '<=' ... logically wired, excluding virtual blocks
								'>>', '<>', '<<' ... signal slicing (any data dependencies)
								}
							
							ps = double.empty(1,0);
							
							virt = ismember(combinator.type, {'~>', '~', '<~', '<<', '<>', '>>'});
							
							% mark frontier in case of signal-slicing traversal mode
							front = info(ismember(combinator.type, {'>>', '<>', '<<'}));
							
							if ismember(combinator.type, {'~>', '~', '=>', '=', '>>', '<>'})
								for port = [-info slQuery.get_ports(info, 'Outport', combinator.sp)]
									ps = [ps slQuery.follow(port, {}, front, virt)]; %#ok<AGROW>
								end
							end
							
							if ismember(combinator.type, {'<~', '~', '<=', '=', '<<', '<>'})
								for port = [-info slQuery.get_ports(info, 'Inport', combinator.sp)]
									ps = [ps slQuery.follow(port, {}, front, virt)]; %#ok<AGROW>
								end
							end
							
							% filter by port handle number
							if ~isempty(combinator.dp)
								if isnumeric(combinator.dp)
									ps = ps(slQuery.get_param(ps, 'PortNumber') == combinator.dp);
								end
							end
							
							% negative return values of follow are the handles of port blocks
							new_col = [-ps(ps < 0)'; slQuery.get_parent(ps(ps>0))];
							new_col = find_system(new_col, 'SearchDepth', 0, find_args{:});
					end
					new_col = unique(new_col, 'rows');
					
					if isnumeric(selector) % selector was a back reference ~> project to rows, where the back ref matches to one of the results
						group = group(ismember(group(:, selector), new_col), :);
					else %if isstruct(selector) ~> append block corresponding to this group
						
						% TODO: perf: move this intersect-step upwards, to avoid unneccesary
						% `find_system`-calls when there aren't any restrictions OR if there is
						% a candidate set, simply test each candidate against the set of
						% restrictions using find_system.
						if ~isempty(selector.argidx)
							new_col = intersect(new_col, double(varargin{str2double(selector.argidx)}), 'rows');
						end
						group = slQuery.combine(group, new_col);
					end
					new_handles = [ new_handles; group ]; %#ok<AGROW>
				end
				
				handles = new_handles;
				if isnumeric(selector)
					hot_col = handles(:, selector);
				else
					hot_col = handles(:, end);
				end
			end
		end
		
		function res = arrayfun(f, varargin)
			% arrayfun a function that returns multiple handles and concat all the results into
			% a single array
			res = arrayfun(f, varargin{:}, 'UniformOutput', false);
			res = [res{:}];
			if isempty(res); res = double.empty(1,0); end
		end
		
		function res = get_param(hs, param)
			% a vectorial get_param without scalar/empty-quirks
			res = arrayfun(@(h) get_param(h, param)', hs, 'UniformOutput', false);
			
			if iscell(res) && ~iscellstr(res)
				res = [res{:}];
			end
			if isempty(res)
				res = double.empty(1,0);
			end
		end
		
		function p = get_parent(b)
			% FIXME: find more performant way than going with the intermediate path
			p = get_param(b, 'Parent');
			if ischar(p)
				if isempty(p)
					p = 0;
				else
					p = get_param(p, 'Handle');
				end
			else
				p = cell2mat(get_param(p, 'Handle'));
			end
		end
		
		function eps = follow(bp, addr, slice, virt)
			% follow the data flow of a port through virtual blocks
			% OutPort/Subsystem, Subsystem/InPort, Goto/From
			
			% bp - the "begin"-port, i.e. the port, whose line we want to follow
			assert(isnumeric(bp) && isscalar(bp));
			% addr - the address of a compound signal (bus, array)
			assert(iscell(addr));
			% signal slicing frontier is a list of blocks, already reached
			assert(isnumeric(slice));
			assert(islogical(virt));

			eps = double.empty(1,0); % only the nonvirtual blocks found during loop

			if bp < 0 % this is a block handle ~> represents non-wired connection
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
				
				% the 'PortType' determines the direction of following (down is true, up is false)
				if strcmp(get_param(bp, 'PortType'), 'inport')
					[edir, pdir, beps] = deal('Outport', 'Inport', slQuery.get_ports(ls, 'SrcPortHandle'));
				else
					[edir, pdir, beps] = deal('Inport', 'Outport', slQuery.get_ports(ls, 'DstPortHandle'));
				end
				
				% eps is the list of "end"-ports of all blocks matched
				if virt
					eps = beps; % use also ports directly adjacent
				end
			end
			
			
			for ep = beps;
				b = slQuery.get_parent(ep);
				% handle all the virtual (signal routing) blocks
				switch get_param(b, 'BlockType')
					case {'Inport', 'Outport'} % a port ~> follow it outside
						s = slQuery.get_parent(b);
						if strcmp(get_param(s, 'Type'), 'block')
							sp = slQuery.get_ports(s, pdir, str2double(get_param(b, 'Port')));
							neps = slQuery.follow(sp, addr, slice, virt);
						else % port is on the highest level ~> cannot go beyond, done
							neps = ep;
						end
						if virt
							neps = [neps -s];
						end
						
					case 'SubSystem' % ~> follow it inside
						if ~ismember(get_param(ep, 'PortType'), {'inport', 'outport'})
							continue;
						end
						pbs = find_system(b, 'LookUnderMasks', 'all', 'FollowLinks', 'on', 'SearchDepth', 1, 'BlockType', edir, 'Port', num2str(get_param(ep, 'PortNumber')));
						neps = slQuery.arrayfun(@(bp) slQuery.follow(bp, addr, slice, virt), slQuery.get_ports(pbs, pdir, 1));
						if virt
							neps = [neps -pbs];
						end
					case 'Goto' % ~> find corresponding From-blocks
						tag_args = {'LookUnderMasks', 'all', 'FollowLinks', 'on', 'GotoTag', get_param(b, 'GotoTag')};
						switch get_param(b, 'TagVisibility')
							case 'local'
								fbs = find_system(slQuery.get_parent(b), 'SearchDepth', 1, tag_args{:}, 'BlockType', 'From');
								
							case 'scoped'
								% find the corresponding scope-block
								s = slQuery.get_parent(b);
								while s
									if ~isempty(find_system(s, 'SearchDepth', 1, tag_args{:}, 'BlockType', 'GotoTagVisibility'))
										break;
									end
									s = slQuery.get_parent(s);
								end
								
								if ~s, continue, end % no scope found ~> no more s-levels, break
								
								% limiting scope block subsystems,
								sbss = find_system(s, tag_args{:}, 'BlockType', 'GotoTagVisibility');
								sbss = slQuery.get_parent(sbss(2:end)); % don't count the actually effective scope block
								
								% now all from blocks but not the ones inside one of sbss, TODO: perf.
								fbs = setdiff( ...
									find_system(s, tag_args{:}, 'BlockType', 'From'), ...
									find_system(sbss, tag_args{:}, 'BlockType', 'From') ...
									, 'rows');
								
							case 'global'
								fbs = find_system(bdroot(b), tag_args{:}, 'BlockType', 'From');
								
						end
						neps = slQuery.arrayfun(@(fp) slQuery.follow(fp, addr, slice, virt), slQuery.get_ports(fbs, 'Outport', 1));
						if virt
							neps = [neps -fbs'];
						end
												
					case 'From' % ~> find corresponding Goto-blocks
						tag_args = {'LookUnderMasks', 'all', 'FollowLinks', 'on', 'GotoTag', get_param(b, 'GotoTag')};
						
						% determine earliest innermost Tag-Visibility
						s = slQuery.get_parent(b);
						% find the corresponding scope-block
						while s
							if ~isempty(find_system(s, 'SearchDepth', 1, tag_args{:}, 'BlockType', 'GotoTagVisibility'))
								break;
							end
							
							s = slQuery.get_parent(s);
						end
						
						if s % there is a matching scope
							% find limiting scope block subsystems
							sbss = find_system(s, tag_args{:}, 'BlockType', 'GotoTagVisibility');
							sbss = slQuery.get_parent(sbss(2:end)); % don't count the actually effective scope block
							
							gbs = setdiff( ...
								find_system(s, tag_args{:}, 'BlockType', 'Goto'), ...
								find_system(sbss, tag_args{:}, 'BlockType', 'Goto') ...
								, 'rows');
							
						else % no scope found ~> local or global Gotos
							gbs = [ ...
								find_system(slQuery.get_parent(b), 'SearchDepth', 1, tag_args{:}, 'TagVisibility', 'local', 'BlockType', 'Goto'), ...
								find_system(bdroot(b), tag_args{:}, 'TagVisibility', 'global', 'BlockType', 'Goto') ...
								];
							
						end
						
						if isempty(gbs), continue, end
						
						if ~isscalar(gbs)
							warning('ambiguous Goto blocks (%s)!', strjoin(gbs, ', '));
						end
						
						neps = slQuery.arrayfun(@(bp) slQuery.follow(bp, addr, slice, virt), slQuery.get_ports(gbs, 'Inport', 1));
						if virt
							neps = [neps -gbs];
						end
						
					case {'Mux', 'Demux'}
						if strcmp(get_param(b, 'BlockType'), 'Mux') == strcmp(edir, 'Inport') % array packing direction ~> add an adr token
							neps = slQuery.follow(slQuery.get_ports(b, pdir, 1), [addr {get_param(ep, 'PortNumber')}], slice, virt);
							
							% ~> array unpacking direction
						elseif isempty(addr) % but there is no addr token for digging down ~> we're done
							neps = ep;
							
						elseif sum(get_param(b, 'Ports')) == 2 % only inport and outport ~> doesn't mux/demux anything at all
							neps = slQuery.follow(slQuery.get_ports(b, pdir, 1), addr, slice, virt);
							%TODO: handle one-dimensional arrays properly
							
						else % a valid addr ~> try to match the array token
							assert(isnumeric(addr{end}));
							neps = slQuery.follow(slQuery.get_ports(b, pdir, addr{end}), addr(1:end-1), slice, virt);
						end
						% case 'Assignment', % case 'Concatenate'
						
					case 'BusCreator'
						bo = get_param(b, 'Object'); names = { bo.Signal.name }; % FIXME: this causes some initialization
						
						if strcmp(edir, 'Inport') % ~> add an address token and follow the outport
							neps = slQuery.follow(slQuery.get_ports(b, 'Outport', 1), [addr names{get(ep, 'PortNumber')}], slice, virt);
							
						elseif isempty(addr) % there is no addr element for digging down the bus ~> we're done
							neps = ep;
							
						else % that addr token must be in the names
							neps = slQuery.follow(slQuery.get_ports(b, 'Inport', strcmp(addr{end}, names)), addr(1:end-1), slice, virt);
							
						end
						
					case 'BusSelector'
						if strcmp(get_param(b, 'OutputAsBus'), 'on') % selector makes a new bus using maybe fewer signals but the same adresses
							neps = slQuery.follow(slQuery.get_ports(b, pdir, 1), addr, slice, virt);
						
						else % must consider signal adresses
							% OutputSignals is a comma-separated list of period-separated signal pathnames
							pathes = strsplit(get_param(b, 'OutputSignals'), ',');
						
							if strcmp(edir, 'Inport') % some of the output signals may match the addr token and we will follow them
								% there is an address try and find the matching set of tokens
								neps = double.empty(1, 0);
								for ti = numel(addr):-1:1
									if ~ischar(addr{ti}), break, end % out of name tokens, cannot go further down
									for spi = find(strcmp(strjoin(addr(end:-1:ti), '.'), pathes)) % indices of ports referencing that exact signal
										neps = [neps slQuery.follow(slQuery.get_ports(b, 'Outport', spi), addr(1:ti-1), slice, virt)]; %#ok<AGROW>
									end
								end
							else % ~> add corresponding name tokens to addr
								tokens = fliplr(strsplit(pathes{get_param(ep, 'PortNumber')}, '.'));
								neps = slQuery.follow(slQuery.get_ports(b, 'Inport', 1), [addr tokens], slice, virt);
							end
						end
						
					case 'BusAssignment'
						pathes = strsplit(get_param(b, 'AssignedSignals'), ',');
						if strcmp(edir, 'Inport') && get_param(ep, 'PortNumber') > 1 % ~> it's one of the substitution element ports
							neps = slQuery.follow(slQuery.get_ports(b, pdir, 1), [addr pathes{get(ep, 'PortNumber') - 1}], slice, virt);
						else % it's the bus port (whole bus)
							if isempty(addr) % there is no addr token for digging down and because original the bus as-such doesn't continue, we're done
								% TODO: does this make any sense?
								neps = ep;
							else
								% is the selected element one of the substituted ones?
								[~, selidx] = ismember(addr{end}, pathes);
								if selidx == 0 % signal wasn't selected ~> follow opposite bus port (pdir)
									neps = slQuery.follow(slQuery.get_ports(b, pdir, 1), addr, slice, virt);
									
									% signal name was substituted
								elseif strcmp(edir, 'Inport') % substitution has cancelled this name downstream
									continue;
								else % follow the respective input port upstream
									neps = slQuery.follow(slQuery.get_ports(b, pdir, selidx + 1), addr(1:end-1), slice, virt);
								end
							end
						end
						
					case {'DataTypeConversion', 'InitialCondition', 'SignalConversion', 'SignalSpecification'} % nonvirtual "through-blocks"
						neps = slQuery.follow(slQuery.get_ports(b, pdir, 1), addr, slice, virt);
						
					otherwise
						if isempty(slice) % no signal slicing ~> this non-virtual endpoint is an endpoint
							neps = ep;
							
						elseif ~ismember(b, slice)
							% signal slicing ~> follow the opposite ports of this endpoint
							bps = slQuery.get_ports(b, pdir); % all ports of the other side
							slice = [slice b]; %#ok<AGROW>
							
							neps = slQuery.arrayfun(@(bp) slQuery.follow(bp, addr, slice, virt), bps);
							
							neps = setdiff([ep neps], eps); % TODO: (why) is this necessary?
							
						else
							neps = [];
						end
				end
				eps = [eps neps]; %#ok<AGROW>
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
			elseif ismember(type, {'SrcPortHandle', 'DstPortHandle'})
				ps = slQuery.get_param(hs, type);
			elseif isempty(hs)
				ps = double.empty(0,1);
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
					ps = ps(slQuery.get_param(ps, 'PortNumber') == index);
				else % char ~> filter by port name, when the block is a subsystem
					if ismember(type, {'Inport', 'DstPortHandle'})
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
			
			if isempty(ps); ps = double.empty(1, 0); end;
		end
		
		function c = combine(a, b)
			% combine all rows of a with all rows of b (like strcat on two cellstrs)
			ha = size(a, 1); hb = size(b, 1);
			c = [ a(repmat(1:ha, hb, 1), :) b(repmat(1:hb, 1, ha), :)];
		end
	end
	methods(Static) % wrappers for convenience
		function result = find(varargin)
			result = slQuery(get_param(find_system(bdroot, varargin{:}), 'Handle'));
		end
		
		function result = gcb
			result = slQuery(gcbh);
		end
		
		function result = gcs
			result = slQuery(get_param(gcs, 'Handle'));
		end
	end
end

% the server crashes if the user's password is a resolvable url
