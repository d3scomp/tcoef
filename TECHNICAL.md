# About

This is a companion page to a paper submitted to ECSA 2017, titled “ADL for Dynamic Coalitions in Smart Cyber-Physical Systems”. It presents an overview of the TCOF library, including short descriptions of the core TCOF concepts and their realization in Scala. In addition, an extended discussion of the advantages and disadvantages of internal vs. external DSL representation is provided, along with the experience gained from developing the external ensemble-based DSL (EDL).

# Project structure
The project consists of the following parts, identified by their path in the folder structure:

`/tcof/src/main/scala/tcof` – core library  
`/tcof/src/main/scala/tcof/traits` – reusable traits  
`/tcof/src/main/scala/rcrs/` - code specific for RCRS domain  
`/tcof/src/main/scala/rcrs/scenario/ProtectScenario` – example scenario with FireFighters  

# Main concepts

The class diagram XXX presents a simplified overview of the tcof library. Some classes/traits are intentionally merged into single class in the diagram (e.g., Ensemble, EnsembleGroup, EnsembleGroupMembers traits are represented by the Ensemble abstract class) for the sake of brevity. The library also contains more non-core reusable traits (tcof.traits) than shown in the diagram.

tcof package contains the following core traits/classes:

_Component_ – represents an autonomic entity which has state and performs periodic activity. Component has sensing, constraints, utility and coordination methods, each taking expression (by-name parameter) and being executed by the tcof library in the following order:
1.	In sensing component senses data from the environment and updates its knowledge model.
2.	State (see below) of the component is resolved so that constraints are satisfied and the objective function described in utility is maximized.
3.	Actuation is performed in coordination.

_State_ – a hierarchical structure representing the current status of a component. Whether a component is in a particular state is determined during constraints resolution. A component can be in multiple states at once, i.e., it can be attempting to perform several activities at the same time. Composite states are implemented using StateSetOr, StateSetAnd classes (subclasses of State, omitted from the diagram). For StateSetOr at least one child state must be satisfied, for StateSetAnd all of them must be satisfied.

_Ensemble_ – represents shared state or a joint goal of a group of components. Ensemble is periodically determined at runtime based on its membership condition (specified in membership method) – a predicate defined over the components’ knowledge – so that the objective function (given in the utility method) is maximized. Current implementation uses a component chosen in advance to establish the ensemble (initiator), and host ensemble resolution. 
Ensembles form a hierarchical structure - members of a sub-ensemble must be members of the parent ensemble too. Ensembles are generally allowed to overlap.

_Role_ – models responsibility assigned to a component within an ensemble. Role methods (e.g. all, some, contains, sum…) can be used within Ensemble.membership and Ensemble.utility to describe constraints of the ensemble. Similar to ensembles, roles also form hierarchical structure and can be therefore decomposed/aggregated.

_Model_ - represents a system based on periodical computation. Model class serves as a base for extension by user scenarios (e.g. FireFightingScenario) – non-core traits (e.g. StateSpaceTrait) usually require that the class they are mixed into extends the Model class.
