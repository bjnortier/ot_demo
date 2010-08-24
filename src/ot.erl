-module(ot).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

op_skip_size({ins, _}) ->
    0;
op_skip_size({del, Chars}) ->
    length(Chars);
op_skip_size({ret, N}) ->
    N.


%%
%% The argument indicates how the size of the undesired segment, so the
%% operation should be split up into segments of size length(Op) - X and X
split_op({del, Chars}, SecondSegmentSize) ->
    Length = length(Chars),
    {{del, lists:sublist(Chars, 1, Length - SecondSegmentSize)},
     {del, lists:sublist(Chars, Length - SecondSegmentSize + 1, SecondSegmentSize)}};
split_op({ret, N}, SecondSegmentSize) ->
    {{ret, N - SecondSegmentSize}, {ret, SecondSegmentSize}};
split_op(Component, _) ->
    throw(io_lib:format("split_op called on unsplittable component, ~~p", [Component])).

apply_op({ins, Chars}) ->
    {{ins, Chars}, {ret, length(Chars)}};

apply_op({ret, N}) ->
    {{ret, N}, {ret, 0}};

apply_op({del, Chars}) ->
    {{del, Chars}, {ret, -length(Chars)}}.


transform(ClientOps, ServerOps) ->
    %%transform(ClientOps, ServerOps, [], [], 0, 0).
    {CDashWithNoOp, SDashWithNoOp} = transform(ClientOps, ServerOps, [], [], 0, 0),
    %% Used foldr here instead of foldl as we're using the faster
    %% list cons function instead of the ++ operator
    {filter_empty(compress(CDashWithNoOp)),
     filter_empty(compress(SDashWithNoOp))}.

transform(ClientOps, ServerOps, CAcc, SAcc, ClientPos, ServerPos) ->
%%     io:format("C: ~~p S: ~~p~~nC': ~~p S': ~~p Cp: ~~p Sp: ~~p~~n", 
%% 	      [ClientOps, ServerOps, CAcc, SAcc, ClientPos, ServerPos]),
    transform_(ClientOps, ServerOps, CAcc, SAcc, ClientPos, ServerPos).


transform_(ClientOps, [Sop|Rem], CAcc, SAcc, ClientPos, ServerPos) when ClientPos > ServerPos ->
    SkipSize = op_skip_size(Sop),
    if
	ServerPos + SkipSize > ClientPos ->
	    {ServerOp1, ServerOp2} = split_op(Sop, ServerPos + SkipSize - ClientPos),
	    transform(ClientOps, [ServerOp1 | [ ServerOp2 | Rem] ], CAcc, SAcc, ClientPos, ServerPos);
	true ->
	    {SopT, CopT} = apply_op(Sop),
	    transform(ClientOps, Rem, CAcc ++ [CopT], SAcc ++ [SopT], ClientPos, ServerPos + SkipSize)
    end;
transform_([Cop|Rem], ServerOps, CAcc, SAcc, ClientPos, ServerPos) ->
    SkipSize = op_skip_size(Cop),
    {CopT, SopT} = apply_op(Cop),
    transform(Rem, ServerOps, CAcc ++ [CopT], SAcc ++ [SopT], ClientPos + SkipSize, ServerPos);
transform_([], [Sop|Rem], CAcc, SAcc, ClientPos, ServerPos) ->
    SkipSize = op_skip_size(Sop),
    {SopT, CopT} = apply_op(Sop),
    transform([], Rem, CAcc ++ [CopT], SAcc ++ [SopT], ClientPos, ServerPos + SkipSize);
%% No more components
transform_([], [], Cdash, Sdash, _, _) ->
    {Cdash, Sdash}.
    

compress(Components) ->
    compress_retains(compress_deletes(Components)).


compress_deletes([]) ->
    [];
compress_deletes([H|Rest]) ->
    compress_deletes(H, Rest, []).

%% The delete-retain(-N) pairs are compressed first, to void interference
%% with other retains within the list of components. These pair have a higher
%% priority than the cancellation of the retains

compress_deletes(Hd, [], Acc) ->
    Acc ++ [Hd];
%% TODO: What about when -N is -1 and length(Chars) == 2?
compress_deletes({ret, N}, [{del, Chars}|Rest], Acc) when -N =:= length(Chars) ->
    compress_deletes({ret, 0}, Rest, Acc);
compress_deletes({del, Chars}, [{ret, N}|Rest], Acc) when -N =:= length(Chars) ->
    compress_deletes({ret, 0}, Rest, Acc);
compress_deletes(Hd, [Next |Rest], Acc) ->
    compress_deletes(Next, Rest, Acc ++ [Hd]).

compress_retains([]) -> 
    [];
compress_retains([H|Rest]) ->
    compress_retains(H, Rest, []).

compress_retains(Hd, [], Acc) ->
    Acc ++ [Hd];
%% TODO: What about when -N is -1 and length(Chars) == 2?
compress_retains({ret, N}, [{del, Chars}|Rest], Acc) when -N =:= length(Chars) ->
    compress_retains({ret, 0}, Rest, Acc);
compress_retains({del, Chars}, [{ret, N}|Rest], Acc) when -N =:= length(Chars) ->
    compress_retains({ret, 0}, Rest, Acc);
compress_retains({ret, N}, [{ret,M} |Rest], Acc) ->
    compress_retains({ret, N+M}, Rest, Acc);
compress_retains(Hd, [Next |Rest], Acc) ->
    compress_retains(Next, Rest, Acc ++ [Hd]).
    
filter_empty(Components) ->
    [X || X <- Components, X /= {ret, 0}].

compress_test() ->
    [
     ?assertMatch([],compress([])),
     ?assertMatch([{ins, "abc"},  {ret, 0}], compress([{ins, "abc"}, {ret, 1}, {ret, -1}])),
     ?assertMatch([{ret,1},{del,"b"}, {ret, 1}],compress([{ret,1},{del,"b"},{ret,2},{ret,-1}])),
     ?assertMatch([{ret,2}], compress([{ret,1},{del,"b"},{ret,-1},{ret,1}])),
     ?assertMatch([{ret,1}], compress([{ret,-1},{del,"b"},{ret,1}])),
     ?assertMatch([{ret, 2}], 
		   compress([{ret,1}, 
			     {ret,0},
			     {del,"X"},
			     {ret,-1},
			     {ret,1},
			     {ret,0}])),
     ?assertMatch([{ret, 2}],
		   compress([{ret,0},
			     {ret,1},
			     {ret,-1},
			     {del,"X"},
			     {ret,0},
			     {ret,1}]))
    ].
    
filter_empty_test() ->
    [
     ?assertMatch([{ins, "abc"}, {ret, 2}], 
		   filter_empty([{ins, "abc"}, {ret, 0}, {ret, 2}]))
    ].

transform_1_test() ->
    ClientComponents = [{ins, "abc"}],
    ServerComponents = [{ins, "def"}],
    [
     ?assertMatch(
	{[{ins, "abc"}, {ret, 3}], [{ret, 3}, {ins, "def"}]},
	transform(ClientComponents, ServerComponents))
    ].

transform_test() ->
    ClientComponents = [{ins, "xyz"}, {ret, 3}],
    ServerComponents = [{ret, 3}, {ins, "def"}],
    [
     ?assertMatch(
	{[{ins, "xyz"}, {ret, 6}],
	 [{ret, 6}, {ins, "def"}]},
	transform(ClientComponents, ServerComponents))
    ].

transform_2b_test() ->
    ClientComponents = [{ins, "abc"}, {ret, 1}, {ins, "def"}],
    ServerComponents = [{ret, 1}],
    [
     ?assertMatch(
	{[{ins, "abc"}, {ret, 1}, {ins, "def"}],
	 [{ret, 7}]},
	transform(ClientComponents, ServerComponents))
    ].

transform_2c_test() ->
    ClientComponents = [{ins, "xyz"}, {ret, 3}],
    ServerComponents = [{ret, 3}, {ins, "def"}],
    [
     ?assertMatch(
	{[{ins, "xyz"}, {ret, 6}],
	 [{ret, 6}, {ins, "def"}]},
	transform(ClientComponents, ServerComponents))
    ].


transform_3_test() ->
    ClientComponents = [{ret, 1}, {del, "b"}, {ret, 1}],
    ServerComponents = [{ret, 2}, {del, "c"}],
    [
     ?assertMatch(
	{[{ret, 1}, {del, "b"}],
	 [{ret, 1}, {del, "c"}]},
	transform(ClientComponents, ServerComponents))
    ].


transform_4_test() ->
    ClientComponents = [{ins, "def"}],
    ServerComponents = [{ins, "abc"}],
    [
     ?assertMatch(
	{[{ins, "def"}, {ret, 3}],
	 [{ret, 3}, {ins, "abc"}]},
	transform(ClientComponents, ServerComponents))
    ].

transform_5_test() ->
    ClientComponents = [{del, "b"}],
    ServerComponents = [{del, "b"}],
    [
     ?assertMatch(
	{[], []}, 
	transform(ClientComponents, ServerComponents))
    ].

transform_7_test() ->
    ClientComponents = [{del, "abc"}, {ret, 3}],
    ServerComponents = [{ret, 3}, {ins, "!"}, {ret, 3}],
    [
     ?assertMatch({[{del, "abc"}, {ret, 4}],
		    [{ins, "!"}, {ret, 3}]},
		   transform(ClientComponents, ServerComponents))
    ].

transform_8_test() ->
    ClientComponents = [{del, "abc"}, {ret, 3}],
    ServerComponents = [{ret, 3}, {ins, "!"}, {ret, 3}],
    [
     ?assertMatch({[{del, "abc"}, {ret, 4}],
		    [{ins, "!"}, {ret, 3}]},
		   transform(ClientComponents, ServerComponents))
    ].


transform_9_test() ->
    ClientComponents = [{ret, 3}, {ins, "X"}, {ret, 3}],
    ServerComponents = [{ret, 2}, {del, "cd"}, {ret, 2}],
    [
     ?assertMatch({[{ret, 2}, {ins, "X"}, {ret, 2}],
		    [{ret, 2}, {del, "c"}, {ret, 1}, {del, "d"}, {ret, 2}]},
		   transform(ClientComponents, ServerComponents))
    ].

transform_10_test() ->
    ClientComponents = [{ret, 1}, {del, "X"}, {ret, 1}],
    ServerComponents = [{ret, 1}, {del, "X"}, {ret, 1}],
    [
     ?assertMatch({[{ret, 2}],
		    [{ret, 2}]},
		   transform(ClientComponents, ServerComponents))
    ].

transform_11_test() ->
    ClientComponents = [{ret, 1}, {ins, "X"}],
    ServerComponents = [{del, "X"}, {ins, "Y"}],
    [
     ?assertMatch({[{ins, "X"},{ret, 1}],
		    [{del, "X"},{ret, 1}, {ins, "Y"}]},
		   transform(ClientComponents, ServerComponents))
    ].


transform_12_test() ->
    ClientComponents = [{ins, "£"}, {ret, 4}, {del, " pounds"}],
    ServerComponents = [{ret, 1}, {del, "000"}, {ret, 7}],
    [
     ?assertMatch({[{ins,"£"},{ret,1},{del," pounds"}],
		    [{ret,2},{del,"000"}]},
		   transform(ClientComponents, ServerComponents))
    ].


segment_test() ->
     [
      ?assertMatch({{ret, 2}, {ret, 1}}, split_op({ret, 3}, 1)),
      ?assertMatch({{ret, 0}, {ret, 3}}, split_op({ret, 3}, 3)),
      ?assertMatch({{del, "c"}, {del, "d"}}, split_op({del, "cd"}, 1)),
      ?assertMatch({{del, "d"}, {del, "ef"}}, split_op({del, "def"}, 2))
     ].


