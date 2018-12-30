-module(word_chains).

-export([word_list/0]).

word_list() ->
    {ok, Data} = file:read_file("words.txt"),
    binary:split(Data, [<<"\n">>], [global]).
