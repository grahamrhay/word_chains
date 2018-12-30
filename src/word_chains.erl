-module(word_chains).

-export([word_list/0, next_words/1, get_word_distance/2]).

word_list() ->
    {ok, Data} = file:read_file("words.txt"),
    lists:map(fun(W) -> binary_to_list(W) end, binary:split(Data, [<<"\n">>], [global])).

next_words(FirstWord) ->
    WordList = word_list(),
    SameLengthWords = lists:filter(fun(W) -> length(W) =:= length(FirstWord) end, WordList),
    WordDistances = lists:map(fun(W) -> {W, get_word_distance(W, FirstWord)} end, SameLengthWords),
    lists:map(fun({Word, _}) -> Word end, lists:filter(fun({_, Distance}) -> Distance =:= 1 end, WordDistances)).

get_word_distance(Word1, Word2) ->
    Differences = lists:zipwith(fun(X, Y) -> case X =:= Y of true -> 0; false -> 1 end end, Word1, Word2),
    lists:foldl(fun(D, Acc) -> Acc + D end, 0, Differences).
