-module(perceptron).
-export([start/0]).

start() ->
	spawn(fun() -> init() end).

init() ->
	receive
		{init, _, InputPids, OutputPids} ->
			io:format("[Perceptron ~w] Initialized~n", [self()]),
			loop(InputPids, OutputPids, rand_weights(InputPids))
	end.

loop(InputPids, OutputPids, InputWeightDict) -> 
	%io:format("[Perceptron ~w] Weights: ~w~n", [self(), dict:to_list(InputWeightDict)]),
	{FiredValue, InputValues} = feed_forward(InputPids, OutputPids, InputWeightDict),
	NewWeightDict = back_propagate(InputPids, OutputPids, InputWeightDict, InputValues, FiredValue),
	loop(InputPids, OutputPids, NewWeightDict).

%
% Forward-feed Functions
%
feed_forward(InputPids, OutputPids, InputWeightDict) -> 
	InputValues = collect_inputs(InputPids),
	FiredValue = output_signal(InputValues, InputWeightDict),
	fire(OutputPids, FiredValue),
	{FiredValue, InputValues}.

% Accumulate values from each input node until all inputs have been received
collect_inputs(InputPids) -> 
	collect_inputs(InputPids, []).
collect_inputs([], InputValues) -> InputValues;
collect_inputs([InputPid|InputPids], InputValues) ->
	receive
		{feedforward, InputPid, Value} ->
			collect_inputs(InputPids, [{InputPid, Value}|InputValues])
	end.

% Calculate the output signal from the inputs
output_signal(InputValues, InputWeightDict) -> 
	output_signal(InputValues, InputWeightDict, 0).
output_signal([], _, Accum) -> 
	sigmoid(Accum);
output_signal([{InputPid, Value}|InputValues], InputWeightDict, Accum) -> 
	Weight = lists:last(dict:fetch(InputPid, InputWeightDict)),
	output_signal(InputValues, InputWeightDict, Accum + Weight * Value).

% Fire the perceptron and send the sigmoid output to each output node
fire([], _) -> [];
fire([OutputPid|OutputPids], FiredValue) ->
	OutputPid ! {feedforward, self(), FiredValue},
	fire(OutputPids, FiredValue).

%
% Backprop Functions
% 
back_propagate(InputPids, OutputPids, InputWeightDict, InputValues, FiredValue) ->
	Err = collect_err(OutputPids),
	Delta = Err * FiredValue * (1.0 - FiredValue),
	Eta = 0.3,
	send_err(InputPids, InputWeightDict, Delta),
	updated_weights(InputValues, InputWeightDict, Delta, Eta).

% Accumulate backprop errors from output nodes
collect_err(OutputPids) -> 
	collect_err(OutputPids, 0).
collect_err([], Accum) -> Accum;
collect_err([OutputPid|OutputPids], Accum) -> 
	receive
		{backprop, OutputPid, PartialErr} ->
			collect_err(OutputPids, Accum + PartialErr)
	end.

% Send backprop error to input nodes
send_err([], _, _) -> [];
send_err([InputPid|InputPids], InputWeightDict, Delta) ->
	Weight = lists:last(dict:fetch(InputPid, InputWeightDict)),
	InputPid ! {backprop, self(), Weight * Delta},
	send_err(InputPids, InputWeightDict, Delta).

updated_weights(InputValues, OldWeights, Delta, Eta) ->
	updated_weights(InputValues, OldWeights, Delta, Eta, dict:new()).
updated_weights([], _, _, _, NewWeights) -> NewWeights;
updated_weights([{InputPid, Value}|InputValues], OldWeights, Delta, Eta, NewWeights) ->
	WeightDelta = Delta * Eta * Value,
	OldWeight = lists:last(dict:fetch(InputPid, OldWeights)),
	NewWeight = OldWeight + WeightDelta,
	updated_weights(InputValues, OldWeights, Delta, Eta, dict:append(InputPid, NewWeight, NewWeights)).


%
% Utility Functions
%

% Sigmoid function
sigmoid(X) -> 
	1.0 / (1.0 + math:exp(-X)).

% Build a random weight map
rand_weights(Keys) -> 
	rand_weights(Keys, dict:new()).
rand_weights([], D) -> D;
rand_weights([Key|Keys], D) -> 
	rand_weights(Keys, dict:append(Key, rgen:rand() - 0.5, D)).
