-module(rgen).
-export([start/0, rand/0]).

start() ->
	Pid = spawn(fun() -> init() end),
	register(rgen, Pid).
	
init() ->
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
	loop().
	
loop() ->
	receive
		{rand, From} ->
			From ! {rand, self(), random:uniform()},
			loop()
	end.
	
rand() ->
	rgen ! {rand, self()},
	receive
		{rand, _, N} -> N
	end.