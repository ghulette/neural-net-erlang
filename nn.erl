-module(nn).
-export([start/3]).

start(NumInputs, NumHidden, NumOutputs) -> 
	HiddenNodes = layer(NumHidden, NumInputs),
	OutputNodes = layer(NumOutputs, NumHidden),
	spawn(fun() -> loop(HiddenNodes, OutputNodes) end).

layer(0, _) -> [];
layer(N, I) -> [perceptron:start(I) | layer(N-1, I)].

propagate(Nodes, Inputs) -> propagate(Nodes, Nodes, Inputs).
propagate([], Nodes, _) -> get_result(Nodes);
propagate([Node|T], Nodes, Inputs) -> 
	Node ! {fire, self(), Inputs},
	propagate(T, Nodes, Inputs).

get_result(Nodes) -> get_result(Nodes, []).
get_result([], Result) -> Result;
get_result([Pid|T], Result) ->
	receive
		{fired, Pid, V} ->
			get_result(T, [V|Result])
	end.

loop(HiddenNodes, OutputNodes) ->
	receive
		{run, From, Inputs} ->
			io:format("Running~n"),
			HV = propagate(HiddenNodes, Inputs),
			OV = propagate(OutputNodes, HV),
			From ! OV,
			loop(HiddenNodes, OutputNodes)
	end.