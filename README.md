# TextWorldExpress
A highly optimized reimplementation of three text game benchmarks focusing on instruction following, commonsense reasoning, and object identification.

# Quickstart
**Before running:** You will have to have `Java 1.8+` installed on your system (shipped with most linux distributions).

Install with pip:
```bash
conda create --name textworld-express python=3.8
conda activate textworld-express
pip install textworld-express orjson
```

Run an example random agent, on TODO...:
> python examples/random_agent.py coin

Run a user console where you can interact with the environment, on TODO...:
> python examples/human.py cookingworld


# Web Server Demo

A web server demo is also available, that allows running a TextWorldExpress user console that can be interacted with in a web browser.

To run the web server demo:
```bash
conda create --name textworld-express python=3.8
conda activate textworld-express
pip install textworld-express[webserver] orjson
```

Run the web server:
> python examples/textworldexpress-web-server.py

Point your web browser to [localhost:8080](https://localhost:8080).


# TextWorldExpress Design
TextWorldExpress is written in Scala (2.12.9), and compiles using `sbt` into a JAR file that is run with `java`. For convenience, a `python` API is provided, which interfaces using the `py4j` package.

**Ports:** TextWorldExpress is nominally run as a server, which interfaces to the Python API with `py4j` through a port. The default port is `25335`.  The actual port used will be 25335 + the thread number provided when a TextWorldExpress class is instantiated.

**Threads:** TextWorldExpress is designed to run many threads simultaneously, if desired. To do so, initialize one `TextWorldExpressEnv` object per thread, and provide a unique `threadNum` for each thread. Don't forget to close down servers that you instantiate using the `env.shutdown()` command.  If you are spawning many threads (10+) at the same time, you may wish to add a short delay (5-10 seconds) after initialization to wait for all the servers to initialize.

## Environments

TextWorldExpress includes high-speed versions of three popular benchmark environments for text-game research.

### CookingWorld
The CookingWorld environment tasks agents with preparing meals by following the instructions in a recipe that is provided in the environment. Agents must first collect required food ingredients (e.g., milk, bell pepper, flour, salt) that can be found in the environment in canonical locations (e.g., kitchen, pantry, supermarket, garden) and containers (e.g., fridge, cupboard). Randomly generated recipes require agents to first use a knife to prepare food by *slicing*, *dicing*, or *chopping* a subset of ingredients, then additionally using an appropriate heating appliance to *fry*, *roast*, or *barbeque* the ingredients. If all ingredients are prepared according to the recipe, the agent can use an action to *prepare the meal*, and finally *eat the meal* to complete the task successfully. Task complexity can be controlled by varying the number of locations in the environment, the number of ingredients required for the recipe, and the number of distractor ingredients randomly placed in the environment that are not required for the recipe. The recipes and environments are parametrically generated, with subsets of ingredients and specific preparations held out between training, development, and test sets to prevent overfitting. CookingWorld was originally created for the [First TextWorld Problems competition](https://aka.ms/ftwp) and later named by [[Madotto etal., 2020]](https://www.ijcai.org/proceedings/2020/207).

### TextWorld Commonsense (TWC)
Text game agents frequently learn the dynamics of environment -- such as the need to open a door before one can move through it -- from interacting with the environment itself, rather than using a pre-existing knowledge base of common sense facts or object affordances that would speed task learning. [TextWorld Commonsense](https://github.com/IBM/commonsense-rl) [[Murugesan etal., 2021]](https://arxiv.org/abs/2010.03790) aims to evaluate agents on common sense knowledge that can not be directly learned from the environment by providing agents a clean-up task where the agent must place common household objects (e.g., *a dirty dish*) in their canonical locations (e.g., *the dishwasher*) that can be found in knowledge bases such as ConceptNet. Separate lists of objects are used in the training, development, and test sets, meaning the agent can not learn object locations from the training set alone, and must rely on an external common sense knowledge base to perform well on the development and test sets. TextWorld Commonsense benchmark has three task difficulty levels, with the easiest including a single location and object to put away, while the hard setting includes up to p to 11 locations and any number of task-relevant and distractor objects.

### Coin Collector
Agents frequently find tasks such as object search, environment navigation, or pick-and-place tasks challenging. The [Coin Collector](https://github.com/xingdi-eric-yuan/TextWorld-Coin-Collector) distills these into a single benchmark where an agent must explore a series of rooms to locate and pick up a single coin. In the original implementation, the game map typically takes the form of a connected loop or chain, such that continually moving to new locations means the agent will eventually discover the coin -- while including medium and hard modes that add in one or more "dead-end" paths.  To control for environment difficulty across games, the TextWorldExpress reimplementation uses the same map generator across environments, and generates arbitrary home environments rather than connected loops. The user maintains control of other measures of difficulty, including the total number of rooms, and the number of distractor objects placed in the environment.
