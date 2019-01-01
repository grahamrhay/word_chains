-module(prop_word_chains).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_all_chains_should_include_last_word() ->
    ?FORALL({FirstWord, LastWord}, valid_words(),
        begin
            Words = word_chains:word_list(length(FirstWord)),
            Chains = word_chains:all_chains(FirstWord, LastWord, Words, length(FirstWord)),
            InvalidChains = lists:filter(fun([W|_]) -> W =/= LastWord end, Chains),
            length(InvalidChains) =:= 0
        end).

valid_words() ->
    ?SUCHTHAT({FirstWord, LastWord},
        ?LET(N, choose(2, 10),
            begin
                WordList = word_chains:word_list(N),
                {random_word(WordList), random_word(WordList)}
            end),
    FirstWord =/= LastWord).

random_word(Words) ->
    lists:nth(rand:uniform(length(Words)), Words).

word_list_test() ->
    WordList = word_chains:word_list(3),
    ?assertEqual(length(WordList), 1311).

word_list_first_word_test() ->
    WordList = word_chains:word_list(2),
    FirstWord = lists:nth(1, WordList),
    ?assertEqual(FirstWord, "aa").

word_list_last_word_test() ->
    WordList = word_chains:word_list(4),
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

all_chains_cat_cat_test() ->
    Words = word_chains:word_list(3),
    Chains = word_chains:all_chains("cat", "cat", Words, 10),
    ?assertEqual([["cat"]], Chains).

all_chains_cat_cot_test_() ->
    {timeout, 5, fun() ->
        Words = word_chains:word_list(3),
        Chains = word_chains:all_chains("cat", "cot", Words, 3),
        ?assertEqual([["cot","cat"],["cot","cit","cat"],["cot","cut","cat"]], Chains)
    end}.
