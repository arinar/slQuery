function dtor = fix_param(bs, p)
if ischar(bs)
	bs = get_param(bs, 'Handle');
elseif iscellstr(bs)
	bs = cellfun(@(b) get_param(b, 'Handle'), bs);
end
vs = arrayfun(@(b) get_param(b, p), bs, 'UniformOutput', false);
dtor = onCleanup(@() arrayfun(@(b, v) set_param(b, p, v{:}), bs, vs));
end
