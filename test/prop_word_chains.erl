-module(prop_word_chains).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_word_chains() ->
    ?FORALL(_, term(),
        begin
            true
        end).

word_list_test() ->
    WordList = word_chains:word_list(),
    ?assertEqual(length(WordList), 274926).

word_list_first_word_test() ->
    WordList = word_chains:word_list(),
    FirstWord = lists:nth(1, WordList),
    ?assertEqual(FirstWord, <<"aa">>).

word_list_last_word_test() ->
    WordList = word_chains:word_list(),
    LastWord = lists:last(WordList),
    ?assertEqual(LastWord, <<"zzzs">>).
