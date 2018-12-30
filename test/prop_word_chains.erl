-module(prop_word_chains).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_next_words_should_be_near() ->
    ?FORALL({FirstWord, LastWord}, valid_words(),
        begin
            io:format("First word: ~p, Last word: ~p", [FirstWord, LastWord]),
            NextWords = word_chains:next_words(FirstWord),
            io:format("Next words: ~p", [NextWords]),
            InvalidWords = lists:filter(fun(W) -> word_chains:get_word_distance(W, FirstWord) =/= 1 end, NextWords),
            io:format("Invalid words: ~p", [InvalidWords]),
            length(InvalidWords) =:= 0
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

get_word_distance_same_word_test() ->
    WordDistance = word_chains:get_word_distance("cat", "cat"),
    ?assertEqual(0, WordDistance).

get_word_distance_near_word_test() ->
    WordDistance = word_chains:get_word_distance("cat", "cot"),
    ?assertEqual(1, WordDistance).

get_word_distance_far_word_test() ->
    WordDistance = word_chains:get_word_distance("cat", "dog"),
    ?assertEqual(3, WordDistance).
