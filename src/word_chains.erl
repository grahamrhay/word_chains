-module(word_chains).

-export([word_list/1, get_word_distance/2, all_chains/4]).

word_list(N) ->
    {ok, Data} = file:read_file("words.txt"),
    WordList = lists:map(fun(W) -> binary_to_list(W) end, binary:split(Data, [<<"\n">>], [global])),
    lists:filter(fun(W) -> length(W) =:= N end, WordList).

all_chains(FirstWord, LastWord, Words, MaxLength) ->
    lists:sort(fun(A, B) -> length(A) =< length(B) end, all_chains(FirstWord, LastWord, Words, MaxLength, [[FirstWord]])).

all_chains(FirstWord, LastWord, Words, MaxLength, Chains) ->
    lists:append(lists:map(fun(Chain) ->
        [CurrentWord | _Rest] = Chain,
        case CurrentWord =:= LastWord of
            true -> [Chain];
            false ->
                NextWords = next_words(CurrentWord, Words),
                NewChains = compact(lists:map(fun(NewWord) ->
                    case lists:member(NewWord, Chain) of
                        false ->
                            NewChain = [NewWord | Chain],
                            case length(NewChain) > MaxLength of
                                true -> [];
                                false -> NewChain
                            end;
                        true -> []
                    end
                end, NextWords)),
                all_chains(FirstWord, LastWord, Words, MaxLength, NewChains)
        end
    end, Chains)).

compact(List) ->
    lists:filter(fun(E) -> length(E) > 0 end, List).

next_words(Word, Words) ->
    WordDistances = lists:map(fun(W) -> {W, get_word_distance(W, Word)} end, Words),
    lists:map(fun({W, _}) -> W end, lists:filter(fun({_, Distance}) -> Distance =:= 1 end, WordDistances)).

get_word_distance(Word1, Word2) ->
    Differences = lists:zipwith(fun(X, Y) -> case X =:= Y of true -> 0; false -> 1 end end, Word1, Word2),
    lists:foldl(fun(D, Acc) -> Acc + D end, 0, Differences).
