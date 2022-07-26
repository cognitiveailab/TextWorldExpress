
# TextWorld
pip install textworld

## (optional) Generate neverending.z8 game (already in there)
python benchmark_textworld.py --max-steps 1000 --nb-objects 50 --nb-rooms 20 --quest-length 5 --quest-breadth 1 --seed 43783

## With valid actions
python benchmark_textworld.py --game gen_games/neverending.z8 --mode random-cmd
python benchmark_textworld.py --game gen_games/neverending.ulx --mode random-cmd

## Without valid actions
python benchmark_textworld.py --game gen_games/neverending.z8 --mode random
python benchmark_textworld.py --game gen_games/neverending.ulx --mode random

## With valid actions
python benchmark_textworld.py --game games/tw-cooking-recipe3+cook+cut+go6-gMrLIV9oTd2t79a.z8 --mode random-cmd
python benchmark_textworld.py --game games/tw-cooking-recipe3+cook+cut+go6-gMrLIV9oTd2t79a.ulx --mode random-cmd

## Without valid actions
python benchmark_textworld.py --game games/tw-cooking-recipe3+cook+cut+go6-gMrLIV9oTd2t79a.z8 --mode random
python benchmark_textworld.py --game games/tw-cooking-recipe3+cook+cut+go6-gMrLIV9oTd2t79a.ulx --mode random


# Jericho
pip install jericho

## Without valid actions
python benchmark_jericho.py ./roms/*

## With valid actions
python benchmark_jericho.py ./roms/* --get-valid

## With valid actions and steps limit
python benchmark_jericho.py ./roms/* --get-valid --limit 10
