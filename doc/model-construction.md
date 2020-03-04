Model Construction Features (Experimental)
==========================================

Adding and Moving Blocks to Subsystems
--------------------------------------

Blocks can be added to existing block diagrams or subsystems blocks using the element-wise 
division operator `./` - reflecting their parent-child relationship. The second operand can be 
either an `slQuery` block handle, plain block handle or block path of an existing block (acting 
as template for the new block) or a specification string to create a new block from a built-in 
one.

```matlab
>> copy = subsystem ./ template
>> new_block = subsystem ./ '〈block-spec〉'
```

The block specification syntax is the same as [basic 
selectors](query-language.md#basic-selectors) except that they now specify the new block's 
paramters instead of search criteria.

The `subsystem` operand can also be a row-vector (list) of subsystem blocks. A new block is 
then inserted into each subsystem in the list.

Equivalently, the `template` operand can also be a column vector (tuple) of blocks, which 
creates one copy for each block of the column at the destination.

When the number of columns in `subsystem` and `template` are the same, the columns of both 
arrays are matched up with each other, and the entire operation is `column-wise`. When the two 
shapes don't match, one of them must be a single-column (allowing insertion of arbitrary arrays 
of blocks into one subsystem or the insertion of one column of blocks into any number of 
subsystems)


Deleting Blocks
---------------


Examples
--------

### Move a block inside of a subsystem

We first find the situations we want to modify, e.g.: data type conversion blocks right outside 
a subsystem shall be moved to the inside.

```matlab
% first query for these situations: get the block, the subsystem it connects to and the
% respective port on the inside
X = slQuery('DataTypeConversion -> SubSystem / Inport <~ $1')

% restrict findings to where the convert-block only connects to the subsystem (can't disturb
% the other destinations) - boolean index for "convert-block has only one sink"
i_coos = cellfun(@isscalar, X(1).LineHandles.Outport.DstPortHandle) 

% unpack actual candidates for easier handling
[C, S, I] = X(i_coos)';

% connect input signal to subystem directly (replaces the existing line)
-1:C - str2double(I.Port):S;

% move the block inside (`/` deletes outside block)
C = S / C;

% connect convert block to current line at input block (deletes the connection to the input)
% and then input port to the convert block
C:1 - I:-1;
I:1 - 1:C;

% guess some good positioning
C.Position = I.Position;
I.Position = I.Position - [50 0 0 0];
```

Granted, when doing these kind of tasks in this manner and style, the notation gets rather 
cryptic and overly concise. But don't think that the modification operators (`/`, `-`) are just 
replacements for `{add,delete}_{block,line}`. There are so many sanity checks and the 
interaction with other features is very thought out.
