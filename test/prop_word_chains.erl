-module(prop_word_chains).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_next_word_should_be_new() ->
    ?FORALL({FirstWord, LastWord}, valid_words(),
        begin
            io:format("First word: ~p, Last word: ~p", [FirstWord, LastWord]),
            NextWord = word_chains:next_word(FirstWord, LastWord),
            io:format("Next word: ~p", [NextWord]),
            NextWord =/= FirstWord
        end).

valid_words() ->
    ?SUCHTHAT({FirstWord, LastWord},
        ?LET(N, choose(2, 10),
            begin
                WordList = word_chains:word_list(),
                SameLengthWords = lists:filter(fun(W) -> length(W) =:= N end, WordList),
                {random_word(SameLengthWords), random_word(SameLengthWords)}
            end),
    FirstWord =/= LastWord).

random_word(Words) ->
    lists:nth(rand:uniform(length(Words)), Words).

word_list_test() ->
    WordList = word_chains:word_list(),
    ?assertEqual(length(WordList), 274926).

word_list_first_word_test() ->
    WordList = word_chains:word_list(),
    FirstWord = lists:nth(1, WordList),
    ?assertEqual(FirstWord, "aa").

word_list_last_word_test() ->
    WordList = word_chains:word_list(),
    LastWord = lists:last(WordList),
    ?assertEqual(LastWord, "zzzs").
