-module(input_node).
-export([start/2]).

start(OutputPids, MainPid) ->
	spawn(fun() -> loop(OutputPids, MainPid) end).

loop(OutputPids, MainPid) ->
	receive
		{feedforward, _, Value} ->
			fire(OutputPids, Value);
		{backprop, _, Value} ->
			MainPid ! {backprop, self(), Value}
	end,
	loop(OutputPids, MainPid).

fire([], _) -> [];
fire([OutputPid|T], Value) -> 
	OutputPid ! {feedforward, self(), Value},
	fire(T, Value).
