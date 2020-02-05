slQuery
=======

the "easy-as-pie API to Simulink"

Introduction
------------

`slQuery` ("slickery") came about, when i realized that Simulink Models are essentially 
structured documents. Considering that nowadays, documents (mostly xml) are being navigated and 
manipulated with suitable and comprehensive mechanisms and especially knowing what the jQuery 
library does so successfully for web developers, this had to be done.

Think of `slQuery` as a powerful syntax for interacting with the blocks of a Simulink at model
construction time (not simulation time). It's a tool for avoiding the heavy boilerplate code
that is the normal API (`find_system`, `get`/`set_param` and any construction built upon them)

How `slQuery` can be used to find simple blocks
-----------------------------------------------

Using _block-selector_ expressions, one can filter all blocks of the model to find, what is 
required or interesting - they are a special syntax for `find_system`-calls. The expression is 
passed as a query to the `slQuery` constructor which will return an array of the resulting 
block handles.

Here are some examples of what can be done - in order of increasing fun.

### Block-Type, Mask-Type

plainly stating a BlockType selects all blocks of that type. The block type selector is the
simplest of selectors.

```matlab
>> slQuery('SubSystem')
   ... finds all simulink subsystems in all models.
```

MaskType selectors can be specified by prefixing the desired mask type with a period.

```matlab
>> slQuery('.TL_BlackBox')
   ... finds blocks masked with `'TL_BlackBox'`
```

Selector parts can be combined to form more specific conditions.

```matlab
>> slQuery('SubSystem.Tl_SimFrame')
   ... find all subsystems masked with `'TL_SimFrame'`, i.e. all TargetLink subsystems.
```

### Block Name

Prefixing a name with a hash (#) selects on a specific block name.

```matlab
>> slQuery('.TL_Inport#in2')
   ... selects TargetLink Inports based on their block name, which must match 'in2'
```

### Conditions for Block/Mask Parameters

Every search can be refined by specifying arbitrary block parameters in brackets. You can use
double quotes ("...") to search for non-simple strings (special chars, whitespace, ...) (TODO)

```matlab
>> slQuery('From[ShowName=on,GotoTag="My Signal"]')
   ... finds From-blocks with the 'ShowName' parameter set to 'on' and a specific 'GotoTag' block parameter
```

There are also special forms of this

```matlab
>> slQuery('From[GotoTag^=My]')               ... starting  with "My"
>> slQuery('From[GotoTag$=Signal]')            ... ending  with "Signal"`
>> slQuery('From[GotoTag*=Sig]')             ... containing  "Sig" anywhere`
>> slQuery('From[GotoTag~="[sS]ig(nal)?s?"]') ... matching  regular expression `[sS]ig(nal)?s?`
```

the `+Flag`-notation is euqivalent to specifying `[Flag=on]`. This way, you can easily filter
for any flag-parameter to be 'on' - only one such flag is allowed though.

```matlab
>> slQuery('+Selected')
   ... finds all currently selected blocks.
```

### Pick from known Set of Blocks

Using `(i)` you may restrict the selection to a candidate set of blocks given as an 
additional argument after the query string. `(i)` then matches only, if the result is also in 
the set of blocks given by the i-th additional argument.

```matlab
>> slQuery('(1)Inport', slQuery('+Selected'))
   ... first find all selected blocks, then pick only Inport-block from that
>> slQuery('Inport+Selected') % equivalent
```

The `(i)`-selection supports handle-arrays, full block pathes given as a `cellstr` or even 
other `slQuery`-results.

```matlab
>> slQuery('(1)', {'sys/port1', 'sys/const1'})
   ... simply transform the given block fullnames to `slQuery`
>> slQuery({'sys/port1', 'sys/const1'}) % equivalent
```

### General Selector

the general form of a selector is this:

```matlab
>> slQuery('(SetIndex)BlockType#BlockName.MaskType+Flag[Param1=Value1,Param2=Value2,...]')
   ... where all parts are optional, but at least one part must be there. You can also use `*`
```

instead of the block type if you simply want to match any block at all.

Selection on situational compounds
----------------------------------

To search for complex model situations, use _combinators_ to describe the relation of two or
more elements matched by block-selectors.

### Hierarchy Combinators

The hierarchy combinator `/` looks for a "direct child"-relation among the two combined
elements.

```matlab
>> slQuery('.TL_SimFrame / Inport')
   ... find Inports directly inside TargetLink Subsystems
```

There are other hierarchy combinators for "direct parents" (`\`), "direct siblings" (` `, 
whitespace)

```matlab
>> slQuery('.TL_SimFrame .TL_MainDialog')
   ... TargetLink Subsystems and TargetLink Main Dialog blocks at the same level

>> slQuery('.TL_MainDialog .TL_MainDialog')
   ... will only match, when there are 2 Main Dialogs
```

There are weaker forms for "arbitrary descendent" (`//`) and "arbitrary ascendent" (`\\`)

### Direct Wiring

The `->`, `<-` and `-` combinators describe dataflow-dependency of the elements. `->` describes 
"feeding", `<-` is "fed by" and `-` is used for either direction.

```matlab
>> slQuery('Inport -> Gain')
   ... find Inports blocks directly connected to a Gain block's input port
```

### Data Flow

The `=>`, `<=` and `=` family of combinators selects on logical data dependency rather than 
explicit wiring in the model. I.e. it includes dependency across routing elements (goto/from, 
ports, arrays, busses) and purely virtual blocks (convert, ...)

```matlab
>> slQuery('Sum => Gain')
   ... find Sum blocks logically feeding into Gain blocks
```

Using `~>`, `<~`, and `~` you can make out all blocks attached to the direct data flow of
a signal. In contrast to `=`, `~` will also match all virtual blocks on the path of data
dependency. This is useful for matching elements at the interfaces of subsystems.
    
```matlab
>> slQuery('Sum ~> *')
   ... find all blocks (including virtual), that are driven by Sum blocks
```

### Data Dependency (Signal Slicing)

Data flow slicing can be used to restrict the search to the set of blocks that are dependent on
a block's output signal.

```matlab
>> slQuery('#MySubsystemLevel / Inport >> #SomeSpecificGain')
   ... find all inports that somehow contribute to a signal of this block

>> slQuery('#MySubsystemLevel / Outport << #SomeSpecificGain')
   ... find all outports that are somehow affected by a signal of this block
```

### Specification of Ports

All signal and data flow oriented combinators (forms of '->', '~>', '=>', '>>') can be extended
by a port specification at either end.

It's possible to annotate a concrete port number to a direct wiring or data flow combinator.

```matlab
>> slQuery('Constant:1 ~> 2:Sum[Sign="+-"]')
... find subtractions by a constant value
```

Although the colon visually binds the port number to the block-selector, the effect should be
considered a property of the combinator. To make this point clear, perhaps read and write it
like this: `'Constant :1~>2: Sum[Sign="+-"]'`

The direction of the port is always derived from the flow-direction of the combinator. In the 
right-pointing combinator flavor ("downstream"), the left hand side port spec specifies the 
output port number of the source block and the right hand side one the input port number of the 
destination block.

When using the bidirectional flavor, an input port always matched up with an output port (only 
that the sense of direction isn't specfied). You cannot find two blocks that are connected to 
the same source like this: `'x:1 - 1:y'`. to find blocks with a common source requires two 
dataflow combinators like so: `'x <- 1: * :1 -> y'`

Trigger ports (Action, or Function-Call) and Enable ports can be matched with the special port
specs `'!:'` and `'?:'`

```matlab
>> slQuery('FunctionCallGenerator ~> !:SubSystem')
   ... find FunctionCallGenerators feeding trigger-Ports
```

Also the port names of Subsystems can be used to reference ports

```matlab
>> slQuery('* -> my_input:SubSystem:my_output -> *')
   ... find blocks connected to special "my_input"-signal ports of subsystems
```

However this syntax can be ambiguous, when using a data flow combinator on either side of a 
simple block-type selector but with only one namey port spec, because both parts are just plain 
words separated by colon with no way to tell, which part should go as the blocktype and which 
as the port-spec.

Fancy Stuff
-----------

### Outer Join

This "any"-relation enables selection of the outer product set of two completely unrelated sets
of situations.

```matlab
>> slQuery('Inport, Outport')
   ... yields all combinations of Inport and Outport selectors
```

This is probably only useful in combination with back reference selectors.

### Back References

A back reference selector `$i`, only matches the exact block already matched by an earlier 
selector in this situational compound: the i-th one. It may only be used proceeding position i.

```matlab
>> slQuery('Inport -> Gain, $1 -> Delay')
   ... yields all inports wired to Gain and the same inports wired to Delay
```

The same information could be retrieved by inverting one of the wiring relations like `'Gain <-
Inport -> Delay'`, but this may not always be practical.

Since the elements matched by a back reference would introduce copies of the elements already
listed previously, they won't be returned in the result. To explain this quirk, you can think of
a normal query, in terms of back-references like so:

```matlab
>> slQuery('Inport -> Gain -> Conversion -> Delay -> Outport')
>> slQuery('Inport, $1 -> Gain, $2 -> Conversion, $3 -> Delay, $4 -> Outport')
```

All block matches from back references will be dropped and the result is then the same.

### Back References in Property Selectors

Back references may be used in propery selectors with the `$i.<parameter>`-syntax, to check
for equality of block parameters.

```matlab
>> slQuery('Goto, From[GotoTag=$1.GotoTag]')
   ... yields all Pairs of Goto- & From-Blocks having the same GotoTag name
```

Although syntactically this is a mode of selection (and not a relation between blocks as 
defined by combinator), this really is fundamentally combinator-like, because it refers to 
another block of the same situational compound.

How results can be used to access stuff
---------------------------------------

The result that a call to `slQuery` returns is a special wrapper for the Simulink handles of
the selected blocks. The wrapper features some nice syntactical sugar for accessing the
parameters of underlying blocks

This is achieved by redirecting the MATLAB subscript syntax (`object.fieldname`) for field
access to the Simulink `get_param` and `set_param` functions and preparating the returned
values in a convenient manner.

```matlab
>> slQuery('Inport').Parent
   'model/subsystem'
   'model/subsystem'
   'model/other/subsystem'
   ... returns the parent Subsystems of Inport blocks at once as a cellstr-array
```

Indexing into the array yields an slQuery-object representing a subselection of handles (as you 
would expect)

```matlab
>> x = slQuery('Constant -> Gain -> Outport');

3x4 slQuery with handles

	10.0003   10.0012   10.0015   10.0026
	11.0003   11.0032   11.0052   11.0077
	10.0002   10.0002   10.0034   10.0034

>> x([2,4], 2:end)

2x2 slQuery with handles

	11.0032   11.0077
	10.0002   10.0034
```

When using only a single index, slQuery predicts based on the type, whether you wand to select
rows or columns: the selectors can be filtered by enumerating the row indices, situation
occurences can be filtered by conditional subscripts.

```matlab
>> x([1,3])

2x4 slQuery with handles

	10.0003   10.0012   10.0015   10.0026
	10.0002   10.0002   10.0034   10.0034

>> x(strcmp(x(2).Value, '1'))

3x2 slQuery with handles

	10.0012   10.0026
	11.0032   11.0077
	10.0002   10.0034
```

Field subscripting can be mixed with indexing and - if the results are compatible - even with
array concatenation.

```matlab
>> slQuery('Constant -> Gain')
>> [ans(1).path, ans(1).Value, ans(2).Gain] ... compose some characteristics of a situation

   'model/subsystem/MyGain1' 'model/subsystem/MyGain2' 'model/other/subsystem/CX' 'model/other/subsystem/CY'
   '3.4'                     '2'                       'C_x'                      'C_y^2'
   '4.8'                     '2.4'                     'K_x'                      '2*K_y'
```

The subscripted assignment to an slQuery result sets all the respective parameter values at
once.

```matlab
>> slQuery('SubSystem')
>> ans.BackGroundColor = 'red'
   ... make all Subsystems red
```

Unfortunately, subscripted assignment isn't possible for function return values in MATLAB, so
this oneliner _won't_ work

```matlab
>> slQuery('SubSystem').BackGroundColor = 'red';
```

When selecting on situational compounds, you can assign a distinct value to the respective
parameters of each of the elements in the situation.

```matlab
>> slQuery('Inport ~> Outport')
>> ans.BackgroundColor = {'blue', 'orange'}
   ... find direct feed-through signals and make their inports blue and outports orange
```

Assignment values can be based off other values in the situation. You can oftentimes keep your
computations totally vectorized and avoid looping over results.
 
```matlab
>> slQuery('Inport -> SignalSpecification -> Outport')
>> ans(1).Name = strcat(ans(2).Name, '_in');
>> ans(3).Name = strcat(ans(2).Name, '_out');
   ... rename inports to `XYZ_in` and outports to `XYZ_out`, this is the MATLAB-way!
```

Interactive Convenience Features
--------------------------------

### Handle and `getfullname`

The most basic block identifiers can be retrieved from an slQuery-result easily

```matlab
>> slQuery('Constant').handle
>> slQuery('Constant').fullname
... produce the (double) handles or full pathes of the selected blocks
```

### Highlighting

Using the special `show`-subsref-Access, you can step through all situations and hilight the
blocks in turn

```matlab
>> slQuery('Constant').show
```

Accessing `showall` will display all situations without wating for user input, resulting in a
short hilite-fest that brings you along the entire result in sequence.

### Matrix of Block Links

```matlab
>> slQuery('Inport \ SubSystem#MySys / Outport').hyperlink
```

Will produce a cellstr-array containing the block names as clickable links, that will bring you
directly to the block in the diagram.
