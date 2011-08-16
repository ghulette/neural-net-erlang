#!/usr/bin/env escript

main(_) ->
	rgen:start(),
	OutputNodes = [perceptron:start()],
	HiddenNodes = [perceptron:start(), perceptron:start(), perceptron:start(), perceptron:start(), perceptron:start(), perceptron:start(), perceptron:start(), perceptron:start(), perceptron:start(), perceptron:start()],
	InputNodes = [input_node:start(HiddenNodes, self()), input_node:start(HiddenNodes, self())],
	init_nodes(OutputNodes, HiddenNodes, [self()]),
	init_nodes(HiddenNodes, InputNodes, OutputNodes),
	train_repeat(InputNodes, OutputNodes, xor_data(), 100000).

xor_data() ->
	[{[0, 0], 0}, {[0, 1], 1}, {[1, 0], 1}, {[1, 1], 0}].

init_nodes([], _, _) -> [];
init_nodes([Node|Nodes], In, Out) ->
	Node ! {init, self(), In, Out},
	init_nodes(Nodes, In, Out).
	
train_repeat(_, _, _, 0) -> [];
train_repeat(InputNodes, OutputNodes, DataSet, N) ->
	io:format("<~w> ", [N]),
	train(InputNodes, OutputNodes, lists:nth(random:uniform(length(DataSet)),DataSet)),
	train_repeat(InputNodes, OutputNodes, DataSet, N-1).

train(_, _, []) -> [];
train(InputNodes, OutputNodes, {InputValues, DesiredOutput}) ->
	Result = run(InputNodes, InputValues),
	io:format("Input: ~w Output: ~w Expected: ~w~n", [InputValues, Result, DesiredOutput]),
	backprop(OutputNodes, DesiredOutput - Result),
	wait_complete(InputNodes).
	
run([InputNode|InputNodes], [Value|Values]) ->
	InputNode ! {feedforward, self(), Value},
	run(InputNodes, Values);
run([], []) -> 
	receive
		{feedforward, _, Value} -> Value
	end.
	
backprop([OutputNode], Err) ->
	OutputNode ! {backprop, self(), Err}.

wait_complete([]) -> [];
wait_complete([InputNode|InputNodes]) ->
	receive
		{backprop, InputNode, _} -> 
			wait_complete(InputNodes)
	end.
