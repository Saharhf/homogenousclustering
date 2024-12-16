README for Simulation Code: NK Landscape and Agent-Based Modeling of Homogeneous Clustering

Overview

This code implements an agent-based model (ABM) designed to simulate collective problem-solving on an NK landscape. It explores how homogeneous clustering impacts the performance of a population divided into two subgroups: a majority and a minority. The model highlights the epistemic benefits and limitations of clustering in contexts marked by power and size asymmetries, as discussed in the associated research paper, "Challenging the Consensus: The Strategic Value of Homogeneous Groups in Collective Problem Solving."

Key Features
NK Landscape Modeling: The simulation uses NK landscapes to represent complex problem spaces. Parameters N (number of variables) and K (interdependencies) define the problem's complexity and the ruggedness of the landscape.

Agent Dynamics:

Agents attempt to optimize their solutions through a mix of exploitation (copying better solutions from neighbors) and exploration (randomly altering their solutions).
Social dynamics, such as trust and conformity pressure, influence agent behavior based on group membership.
Homogeneous Clustering:

Agents are grouped into two subpopulations (majority and minority).
The model compares scenarios with clustering (group members primarily interact within their group) and random distribution (maximal intermixing of agents).
Simulation Objectives:

Evaluate the impact of clustering on the likelihood of the population converging on the superior solution.
Test various conditions, including size asymmetry, trust, conformity, and the stages of consensus formation.
Dependencies
The code is designed for NetLogo, a multi-agent programmable modeling environment. Ensure you have NetLogo 6.x or later installed.

Extensions Required
array
palette
How to Use
Setup:

Run the setup procedure to initialize the environment, including the NK landscape, agent populations, and network structures.
Simulation:

Use the go command to run the simulation. The process continues until all agents converge on a solution or no further improvements can be made.
Customization:

Parameters such as the number of agents, trust levels, conformity pressure (kappa), and network structure can be adjusted to explore various scenarios.
Outputs:

Key results include:
Time-to-Zero: The number of ticks required for the population to reach consensus.
Final Score: The quality of the solution adopted by the population.
Number of Unique Solutions: A measure of diversity within the population.
Key Contributions
This code extends existing NK landscape models by:

Incorporating social dynamics like trust and conformity, influenced by group membership.
Allowing for asymmetric clustering, where one group's solution may dominate due to structural advantages.
Differentiating the effects of clustering during exploration periods versus post-consensus dynamics.
Citation
If you use this code, please cite the accompanying paper:

Heydari Fard, S. (2024). Challenging the Consensus: The Strategic Value of Homogeneous Groups in Collective Problem Solving.

Future Directions
Introducing additional error factors and noise specific to group interactions.
Allowing agents to change multiple solution bits simultaneously for broader exploration.
Modeling dynamic communication networks where agents can change neighbors over time.
