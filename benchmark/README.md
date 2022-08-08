
# TextWorld

    pip install textworld

### Unzip games

    unzip games.zip

### Generate neverending.z8 game

    python benchmark_textworld.py --max-steps 1000 --nb-objects 50 --nb-rooms 20 --quest-length 5 --quest-breadth 1 --seed 43783 --mode random

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

### Download games

Download [zip file](https://minhaskamal.github.io/DownGit/#/home?url=https:%2F%2Fgithub.com%2FBYU-PCCL%2Fz-machine-games%2Ftree%2Fmaster%2Fjericho-game-suite&rootDirectory=roms) and save it to the `benchmark/` folder.

    unzip jericho-game-suite.zip

## Without valid actions

    python benchmark_jericho.py ./roms/*

## With valid actions

    python benchmark_jericho.py ./roms/* --get-valid

## With valid actions and steps limit

    python benchmark_jericho.py ./roms/* --get-valid --limit 10


# ScienceWorld (TODO)
pip install scienceworld

## With valid actions
python benchmark_scienceworld.py


# TextWorldExpress (TODO)
pip install textworld-express

## With valid actions
python benchmark_textworld_express.py
