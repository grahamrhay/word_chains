-module(word_chains).

-export([word_list/0, next_word/2]).

word_list() ->
    {ok, Data} = file:read_file("words.txt"),
    lists:map(fun(W) -> binary_to_list(W) end, binary:split(Data, [<<"\n">>], [global])).

next_word(_FirstWord, LastWord) ->
    LastWord.
